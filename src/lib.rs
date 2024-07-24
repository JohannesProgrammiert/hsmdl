//! # Hierarchical State Machine Description Language (HSMDL)
//!
//! Parses a HSMDL YAML into Rust objects.
//!
//! Use Hsm::emit_statig() to generate 'statig' Rust code that corresponds the specified state machine.
use codegen::Scope;
use convert_case::{Case, Casing};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;
use std::path::Path;

/// Action specification.
#[derive(Debug, Deserialize)]
pub struct Action {
    pub name: String,
    pub variant: Option<String>,
}

/// Transition from a state to another.
///
/// A 'Transition'-object is owned by the source 'State'-object.
#[derive(Debug, Deserialize)]
pub struct Transition {
    pub event: String,
    pub next: String,
    pub actions: Option<Vec<Action>>,
    pub guards: Option<Vec<GuardCondition>>,
    #[serde(rename = "timing-constraint", default)]
    pub timing_constraint: Option<String>,
    pub description: Option<String>,
}

/// All possible transitions associated to one source state + one event.
pub type TransitionMap<'a> = HashMap<String, Vec<&'a Transition>>;

/// A condition that guards a transition.
#[derive(Debug, Deserialize)]
pub struct GuardCondition {
    pub name: String,
    pub condition: bool,
}

impl GuardCondition {
    pub fn to_uml(&self) -> String {
        let cond = if self.condition { "" } else { "not " };
        format!("{}{}", cond, self.name)
    }
}

pub fn guard_conditions_to_uml(conds: &Vec<GuardCondition>) -> String {
    let mut ret = String::new();
    if conds.len() >= 1 {
        ret = conds[0].to_uml();
    }
    for i in 1..conds.len() {
        ret = format!("{} && {}", ret, conds[i].to_uml());
    }
    ret
}

/// A state may always be the initial state of its super-state or only if some condition is fulfilled.
#[derive(Debug, Clone)]
pub enum InitialSpec<'a> {
    Always,
    OnlyIf(&'a Vec<GuardCondition>),
}

/// Separates state into hierarchical and leaf/simple states.
#[derive(Debug, Clone)]
pub enum StateType<'a> {
    Hierarchical {
        states: &'a Vec<State>,
        exit: &'a String,
    },
    Leaf(&'a Vec<Transition>),
}

/// Specification of a state.
#[derive(Debug, Deserialize)]
pub struct State {
    pub name: String,
    /// Namespace is used to track the state hierarchy.
    #[serde(skip)]
    pub namespace: String,
    initial: Option<bool>,
    initial_if: Option<Vec<GuardCondition>>,
    #[serde(rename = "entry-actions", default)]
    pub entry_actions: Option<Vec<Action>>,
    #[serde(rename = "exit-actions", default)]
    pub exit_actions: Option<Vec<Action>>,
    transitions: Option<Vec<Transition>>,
    states: Option<Vec<State>>,
    exit: Option<String>,
}

impl State {
    /// Collect all actions and their variants in this state and sub states.
    pub fn collect_actions(&self) -> ActionMap {
        let mut actions = ActionMap::new();
        if let Some(entry_actions) = &self.entry_actions {
            for action in entry_actions {
                actions.register_action(action);
            }
        }
        if let Some(exit_actions) = &self.exit_actions {
            for action in exit_actions {
                actions.register_action(action);
            }
        }
        match self.state_type() {
            StateType::Hierarchical { states, .. } => {
                for state in states {
                    actions.merge(state.collect_actions());
                }
            }
            StateType::Leaf(..) => {
                for t in self.transitions.as_ref().unwrap() {
                    if let Some(transition_actions) = &t.actions {
                        for action in transition_actions {
                            actions.register_action(action);
                        }
                    }
                }
            }
        }
        actions
    }

    /// Return namespaced name of this state.
    pub fn namespaced_name(&self) -> String {
        format!("{}{}", self.namespace, self.name)
    }

    /// Collect all guard conditions that occur in transitions and initial_if of this state and sub-states.
    pub fn collect_guards(&self) -> HashSet<&String> {
        let mut collected_guards = HashSet::new();
        match self.state_type() {
            StateType::Hierarchical { states, .. } => {
                for substate in states {
                    for g in substate.collect_guards() {
                        collected_guards.insert(g);
                    }
                }
            }
            StateType::Leaf(transitions) => {
                for t in transitions {
                    if let Some(guards) = &t.guards {
                        for g in guards {
                            collected_guards.insert(&g.name);
                        }
                    }
                }
            }
        }
        if let Some(initial) = self.initial() {
            if let InitialSpec::OnlyIf(guards) = initial {
                for g in guards {
                    collected_guards.insert(&g.name);
                }
            }
        }
        collected_guards
    }

    /// Find a state whose namespaced_name matches the specified one.
    pub fn query_state(&self, namespaced_name: &str) -> Option<&State> {
        if self.namespaced_name() == namespaced_name {
            return Some(&self);
        }
        match self.state_type() {
            StateType::Hierarchical { states, .. } => {
                for substate in states {
                    if let Some(s) = substate.query_state(namespaced_name) {
                        return Some(s);
                    }
                }
                return None;
            }
            StateType::Leaf(..) => {
                return None;
            }
        }
    }

    /// Return all occurring events.
    pub fn collect_events(&self) -> HashSet<&String> {
        let mut collected_events = HashSet::new();
        match self.state_type() {
            StateType::Hierarchical { states, .. } => {
                for substate in states {
                    for g in substate.collect_events() {
                        collected_events.insert(g);
                    }
                }
            }
            StateType::Leaf(transitions) => {
                for t in transitions {
                    collected_events.insert(&t.event);
                }
            }
        }
        collected_events
    }

    /// Return whether this state is the 'initial' state of the superstate.
    pub fn initial(&self) -> Option<InitialSpec> {
        if let Some(initial_conds) = &self.initial_if {
            Some(InitialSpec::OnlyIf(&initial_conds))
        } else if let Some(always_initial) = self.initial {
            if always_initial {
                Some(InitialSpec::Always)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// For superstates, return initial sub states.
    /// For leaf states, this simply returns itself.
    pub fn initial_states(&self) -> Vec<&State> {
        match self.state_type() {
            StateType::Hierarchical { states, .. } => {
                let mut initial_states = Vec::new();
                for state in states {
                    if let Some(initial_spec) = state.initial() {
                        match initial_spec {
                            InitialSpec::Always => return vec![state],
                            InitialSpec::OnlyIf(..) => initial_states.push(state),
                        }
                    }
                }
                initial_states
            }
            StateType::Leaf(..) => {
                vec![&self]
            }
        }
    }

    /// Return state type and the associated properties.
    pub fn state_type(&self) -> StateType {
        if let Some(states) = &self.states {
            return StateType::Hierarchical {
                states,
                exit: self.exit.as_ref().unwrap(),
            };
        } else {
            if let Some(transitions) = &self.transitions {
                return StateType::Leaf(transitions);
            } else {
                // catched by 'Self::validate'
                unreachable!()
            }
        }
    }

    /// Get transitions unique by event type.
    pub fn transition_map_event(&self) -> TransitionMap {
        let mut transition_map = HashMap::new();
        if let StateType::Leaf(transitions) = self.state_type() {
            for t in transitions {
                let entry = transition_map.entry(t.event.clone()).or_insert(Vec::new());
                entry.push(t);
            }
        }
        transition_map
    }

    /// Get transitions unique by next state.
    pub fn transition_map_dest(&self) -> TransitionMap {
        let mut transition_map = HashMap::new();
        if let StateType::Leaf(transitions) = self.state_type() {
            for t in transitions {
                let entry = transition_map.entry(t.next.clone()).or_insert(Vec::new());
                entry.push(t);
            }
        }
        transition_map
    }

    /// Return 'true' if either initial is true or initial_if has some guard condition specified.
    fn has_initial(&self) -> bool {
        self.initial_if.is_some()
            || if let Some(initial) = self.initial {
                initial
            } else {
                false
            }
    }

    /// Do some integrity checks for the parsed data.
    pub fn validate(&self) -> Result<(), HsmParserError> {
        if self.transitions.is_none() && self.states.is_none() {
            return Err(HsmParserError::MissingKey((
                format!("State {}", self.name),
                "transitions ^ states".to_owned(),
            )));
        }
        if self.transitions.is_some() && self.states.is_some() {
            return Err(HsmParserError::InvalidKey((
                format!("Self {}", self.name),
                "transitions and states".to_owned(),
            )));
        }
        if self.states.is_some() && self.exit.is_none() {
            return Err(HsmParserError::MissingKey((
                format!("State {}", self.name),
                "exit".to_owned(),
            )));
        }
        if let Some(sub_states) = &self.states {
            let mut initial_found = false;
            for sub_state in sub_states {
                sub_state.validate()?;
                if sub_state.has_initial() {
                    initial_found = true;
                }
            }
            if !initial_found {
                return Err(HsmParserError::NoInitial(format!("State {}", self.name)));
            }
        }
        Ok(())
    }
    fn emit_puml(&self) -> Vec<String> {
        let mut ret = Vec::new();
        if let Some(i) = self.initial() {
            match i {
                InitialSpec::Always => {
                    ret.push(format!("[*] --> {}", self.name.to_case(Case::UpperSnake)));
                }
                InitialSpec::OnlyIf(g) => {
                    ret.push(format!(
                        "[*] --> {} : {}",
                        self.name.to_case(Case::UpperSnake),
                        guard_conditions_to_uml(g)
                    ));
                }
            }
        }
        match self.state_type() {
            StateType::Hierarchical { states: _, exit } => {
                // sub-self machines have only one transition: the one that appears under 'exit'
                ret.push(format!(
                    "{} --> {}",
                    self.name.to_case(Case::UpperSnake),
                    exit.to_case(Case::UpperSnake)
                ));
            }
            StateType::Leaf(_) => {
                for transition in self.transition_map_dest() {
                    if transition.0 == "exit" {
                        ret.push(format!(
                            "{} --> [*] : {}",
                            self.name.to_case(Case::UpperSnake),
                            transition.1[0].event,
                        ));
                    } else {
                        ret.push(format!(
                            "{} --> {} : {}",
                            self.name.to_case(Case::UpperSnake),
                            transition.0.to_case(Case::UpperSnake),
                            transition.1[0].event
                        ));
                    }
                    if let Some(c) = &transition.1[0].timing_constraint {
                        // timing constraint marker.
                        ret.push("note on link".into());
                        ret.push(format!("     ! ${}", c));
                        ret.push("end note".into());
                    }
                }
            }
        }
        if let Some(entry_actions) = &self.entry_actions {
            for action in entry_actions {
                ret.push(format!(
                    "{} : Entry: {}",
                    self.name.to_case(Case::UpperSnake),
                    action.name
                ))
            }
        }
        if let Some(exit_actions) = &self.exit_actions {
            for action in exit_actions {
                ret.push(format!(
                    "{} : Exit: {}",
                    self.name.to_case(Case::UpperSnake),
                    action.name
                ))
            }
        }
        if let StateType::Hierarchical { states, .. } = self.state_type() {
            ret.push(format!("state {} {{", self.name.to_case(Case::UpperSnake)));
            for s in states {
                ret.extend(s.emit_puml());
            }
            ret.push("}".into());
        }
        ret
    }
}

/// Type of actions; either simple or parametrized by a variant.
#[derive(Debug, Clone)]
enum ActionType {
    Simple,
    Parametrized { variants: HashSet<String> },
}

impl ActionType {
    pub fn register_variant(&mut self, variant: &String) -> Result<(), ()> {
        if let Self::Parametrized { variants } = self {
            variants.insert(variant.clone());
            Ok(())
        } else {
            Err(())
        }
    }
}

/// All associated variants of some action.
#[derive(Debug, Clone)]
pub struct ActionMap {
    map: HashMap<String, ActionType>,
}

impl ActionMap {
    /// Create action spec
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// register an action, i.e. adding its variant.
    fn register_action(&mut self, action: &Action) {
        if !self.map.contains_key(&action.name) {
            // create new entry
            let action_type = if let Some(variant) = &action.variant {
                ActionType::Parametrized {
                    variants: HashSet::from([variant.clone()]),
                }
            } else {
                ActionType::Simple
            };
            self.map.insert(action.name.clone(), action_type);
        } else {
            let entry = self.map.get_mut(&action.name).unwrap();
            if let Some(variant) = &action.variant {
                entry
                    .register_variant(&variant)
                    .expect("Cannot add variant to action previously specified as 'Simple'");
            }
        }
    }

    /// Merge ActionMaps, not allowing duplicates.
    fn merge(&mut self, other: Self) {
        for (other_name, other_action_type) in other.map.iter() {
            let entry = self
                .map
                .entry(other_name.clone())
                .or_insert(other_action_type.clone());
            match other_action_type {
                ActionType::Parametrized {
                    variants: other_variants,
                } => {
                    for variant in other_variants {
                        entry.register_variant(variant).expect(
                            "Cannot add variant to action previously specified as 'Simple'",
                        );
                    }
                }
                _ => {}
            }
        }
    }
}

#[derive(Debug)]
pub enum HsmParserError {
    OpenFile(std::io::Error),
    ReadString(std::io::Error),
    ParseYaml(Box<dyn std::error::Error>),
    MissingKey((String, String)),
    NoInitial(String),
    InvalidKey((String, String)),
}

/// The main handle that holds information about states.
#[derive(Debug, Deserialize)]
pub struct Hsm {
    pub states: Vec<State>,
}

impl Hsm {
    /// Initialize from YAML specification.
    pub fn from_yaml_file(file_path: impl AsRef<Path>) -> Result<Hsm, HsmParserError> {
        let mut file = File::open(file_path).map_err(|e| HsmParserError::OpenFile(e))?;
        let mut contents = String::new();
        file.read_to_string(&mut contents)
            .map_err(|e| HsmParserError::ReadString(e))?;
        let mut inst: Hsm =
            serde_yaml::from_str(&contents).map_err(|e| HsmParserError::ParseYaml(Box::new(e)))?;
        for state in &mut inst.states {
            Self::resolve_namespaces(state);
        }
        inst.validate()?;
        Ok(inst)
    }

    /// Return overall HashMap of action names mapped to occuring variants associated with that action.
    fn actions(&self) -> ActionMap {
        let mut actions = ActionMap::new();
        for state in &self.states {
            actions.merge(state.collect_actions());
        }
        actions
    }

    /// Return all occuring guard names.
    fn guards(&self) -> HashSet<String> {
        let mut guards = HashSet::new();
        for state in &self.states {
            for g in state.collect_guards() {
                guards.insert(g.clone());
            }
        }
        guards
    }

    /// Return all occurring events.
    fn events(&self) -> HashSet<String> {
        let mut events = HashSet::new();
        for state in &self.states {
            for e in state.collect_events() {
                events.insert(e.clone());
            }
        }
        events
    }

    /// Return reference to initial state of state machine.
    pub fn initial(&self) -> &State {
        for state in &self.states {
            if let Some(b) = state.initial {
                if b {
                    return state;
                }
            }
        }
        // self.validate catches this..
        unreachable!();
    }

    /// Do some integrity checks.
    fn validate(&self) -> Result<(), HsmParserError> {
        let mut initial_found = false;
        for state in &self.states {
            state.validate()?;
            if let Some(b) = state.initial {
                if b {
                    initial_found = true;
                }
            }
        }
        if !initial_found {
            return Err(HsmParserError::NoInitial("states".to_string()));
        }
        Ok(())
    }

    /// Populate the state.namespace_name fields by tracking the hierarchy.
    fn resolve_namespaces(state: &mut State) {
        if let Some(sub_states) = &mut state.states {
            for sub_state in sub_states {
                sub_state.namespace = format!("{}{}_", state.namespace, state.name);
                Self::resolve_namespaces(sub_state);
            }
        }
    }

    /// Find a state in the specified namespace.
    ///
    /// To find a state as top-level, set `namespace` to "".
    ///
    /// In case the specified state in "exit", the superstates's "exit"-state is returned
    /// In case the found state is a super-state, the initial states are returned.
    ///
    /// This used to resolve the 'next' field in transitions to a State object.
    fn query_state(&self, namespace: &str, name: &str) -> Vec<&State> {
        let mut result = Vec::new();
        for state in &self.states {
            if name == "exit" {
                if let Some(s) = state.query_state(&namespace[..namespace.len() - 1]) {
                    let exit_state = self.query_state(&s.namespace, s.exit.as_ref().unwrap())[0];
                    result.extend(exit_state.initial_states());
                    break;
                }
            } else {
                let query = &format!("{}{}", namespace, name);
                if let Some(s) = state.query_state(query) {
                    result.extend(s.initial_states());
                    break;
                }
            }
        }
        result
    }

    /// Generate a state method as expected by the 'statig' crate.
    fn statig_state_fn(
        &self,
        fsm_impl: &mut codegen::Impl,
        state: &State,
        superstate: Option<&String>,
    ) {
        match state.state_type() {
            StateType::Hierarchical { states, .. } => {
                let attr: String = if let Some(s) = superstate {
                    format!(
                    "superstate(superstate = \"{}\", entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                    s, state.namespaced_name(), state.namespaced_name()
                )
                } else {
                    format!(
                        "superstate(entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                        state.namespaced_name(),
                        state.namespaced_name()
                    )
                };
                fsm_impl
                    .new_fn(&state.namespaced_name())
                    .attr(&attr)
                    .arg_ref_self()
                    .arg("event", "&Event")
                    .ret("Response<State>")
                    .line("match event {")
                    .line("_ => return Response::Handled,")
                    .line("}");
                for substate in states {
                    self.statig_state_fn(fsm_impl, substate, Some(&state.name));
                }
            }
            StateType::Leaf(..) => {
                let attr: String = if let Some(s) = superstate {
                    format!(
                    "state(superstate = \"{}\", entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                    s, state.namespaced_name(), state.namespaced_name()
                )
                } else {
                    format!(
                        "state(entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                        state.namespaced_name(),
                        state.namespaced_name()
                    )
                };
                let state_fn = fsm_impl
                    .new_fn(&state.namespaced_name())
                    .attr(&attr)
                    .arg_mut_self()
                    .arg("event", "&Event")
                    .ret("Response<State>")
                    .line("match event {");
                for (event, transitions) in state.transition_map_event().iter() {
                    state_fn.line(format!("Event::{} => {{", event.to_case(Case::UpperCamel)));
                    let mut make_default = false;
                    for t in transitions {
                        if let Some(guards) = &t.guards {
                            state_fn.line("if");
                            state_fn.line(format!(
                                "self.guards.{} == {}",
                                guards[0].name, guards[0].condition
                            ));
                            for cond in &guards[1..] {
                                state_fn.line(format!(
                                    "&& self.guards.{} == {}",
                                    cond.name, cond.condition
                                ));
                            }
                            state_fn.line("{");
                            make_default = true;
                        }
                        if let Some(actions) = &t.actions {
                            for action in actions {
                                if let Some(variant) = &action.variant {
                                    let variant_typename = format!("{}_variant", action.name)
                                        .to_case(Case::UpperCamel);
                                    state_fn.line(format!(
                                        "self.actions.{}({}::{});",
                                        action.name,
                                        variant_typename,
                                        variant.to_case(Case::UpperCamel)
                                    ));
                                } else {
                                    state_fn.line(format!("self.actions.{}();", action.name));
                                }
                            }
                        }
                        let next_states = self.query_state(&state.namespace, &t.next);
                        for next in next_states {
                            if let Some(initial) = next.initial() {
                                match initial {
                                    InitialSpec::Always => {
                                        state_fn.line(format!(
                                            "return Response::Transition(State::{}())",
                                            next.namespaced_name()
                                        ));
                                    }
                                    InitialSpec::OnlyIf(guards) => {
                                        state_fn.line("if");
                                        state_fn.line(format!(
                                            "self.guards.{} == {}",
                                            guards[0].name, guards[0].condition
                                        ));
                                        for cond in &guards[1..] {
                                            state_fn.line(format!(
                                                "&& self.guards.{} == {}",
                                                cond.name, cond.condition
                                            ));
                                        }
                                        state_fn.line(format!(
                                            "{{ return Response::Transition(State::{}()) }}",
                                            next.namespaced_name()
                                        ));
                                        make_default = true;
                                    }
                                }
                            } else {
                                state_fn.line(format!(
                                    "return Response::Transition(State::{}())",
                                    next.namespaced_name()
                                ));
                            }
                        }
                        if t.guards.is_some() {
                            state_fn.line("}"); // close guards 'if' clause
                        }
                    }
                    // default needed for guarded transitions in case no guard evaluates to 'true'.
                    if make_default {
                        state_fn.line("return Response::Handled;");
                    }
                    state_fn.line(format!("}},"));
                }
                state_fn.line("_ => Response::Handled,").line("}");
            }
        }
        let entry_fn = fsm_impl
            .new_fn(&format!("{}_entry", state.namespaced_name()))
            .attr("action")
            .arg_mut_self();
        if let Some(entry_actions) = &state.entry_actions {
            for action in entry_actions {
                if let Some(variant) = &action.variant {
                    let variant_typename =
                        format!("{}_variant", action.name).to_case(Case::UpperCamel);
                    entry_fn.line(format!(
                        "self.actions.{}({}::{});",
                        action.name,
                        variant_typename,
                        variant.to_case(Case::UpperCamel)
                    ));
                } else {
                    entry_fn.line(format!("self.actions.{}();", action.name));
                }
            }
        }
        let exit_fn = fsm_impl
            .new_fn(&format!("{}_exit", state.namespaced_name()))
            .attr("action")
            .arg_mut_self();
        if let Some(exit_actions) = &state.exit_actions {
            for action in exit_actions {
                if let Some(variant) = &action.variant {
                    let variant_typename =
                        format!("{}_variant", action.name).to_case(Case::UpperCamel);
                    exit_fn.line(format!(
                        "self.actions.{}({}::{});",
                        action.name,
                        variant_typename,
                        variant.to_case(Case::UpperCamel)
                    ));
                } else {
                    exit_fn.line(format!("self.actions.{}();", action.name));
                }
            }
        }
    }

    pub fn emit_puml(&self, caption: &str) -> String {
        let mut ret = Vec::new();
        ret.push("DO NOT EDIT: Automatically generated".into());
        ret.push("@startuml diagram".into());
        ret.push("scale 1920 width".into());
        ret.push(format!("caption {}", caption));
        for state in &self.states {
            ret.extend(state.emit_puml());
        }
        ret.push("@enduml".into());
        ret.join("\n")
    }

    /// Generate 'statig' state machine code that corresponds the state machine specified in the YAML file.
    pub fn emit_statig(&self) -> String {
        let mut output = Scope::new();
        output.import("statig", "state_machine");
        output.import("statig", "Response");
        let guards = output
            .new_struct("Guards")
            .vis("pub")
            .derive("Debug")
            .derive("Clone")
            .derive("Copy")
            .derive("PartialEq")
            .derive("Eq");
        for guard in self.guards() {
            let mut field = codegen::Field::new(&guard, "bool");
            field.vis("pub");
            guards.push_field(field);
        }
        let mut actions = codegen::Trait::new("Actions");
        actions.vis("pub");
        for (action_name, action_type) in self.actions().map.iter() {
            let func = actions.new_fn(action_name).arg_mut_self();
            if let ActionType::Parametrized { variants } = &action_type {
                let variant_typename = format!("{}_variant", action_name).to_case(Case::UpperCamel);
                func.arg("variant", variant_typename.clone());
                let variant_enum = output
                    .new_enum(variant_typename)
                    .vis("pub")
                    .derive("Debug")
                    .derive("Clone")
                    .derive("Copy")
                    .derive("PartialEq")
                    .derive("Eq");
                for var in variants {
                    variant_enum.new_variant(var.to_case(Case::UpperCamel));
                }
            }
        }
        output.push_trait(actions);
        output
            .new_struct("Fsm")
            .vis("pub")
            .generic("T")
            .bound("T", "Actions")
            .derive("Debug")
            .field("guards", "Guards")
            .field("actions", "T");
        let fsm_impl = output
            .new_impl("Fsm")
            .r#macro(&format!(
                "#[state_machine(initial = \"State::{}()\")]",
                self.initial().name
            ))
            .generic("T")
            .bound("T", "Actions")
            .target_generic("T");
        fsm_impl
            .new_fn("new")
            .vis("pub")
            .arg("guards", "Guards")
            .arg("actions", "T")
            .ret("Self")
            .line("Self { guards, actions }");
        for state in &self.states {
            self.statig_state_fn(fsm_impl, state, None);
        }
        let events = output
            .new_enum("Event")
            .vis("pub")
            .derive("Debug")
            .derive("PartialEq")
            .derive("Eq")
            .derive("Clone")
            .derive("Copy");
        for event in self.events() {
            events.push_variant(codegen::Variant::new(event.to_case(Case::UpperCamel)));
        }
        output.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let hsm = Hsm::from_yaml_file("test.yaml").expect("YAML deser failed");
        let statig = hsm.emit_statig();
        println!("{}", statig);
    }
}
