use codegen::Scope;
use convert_case::{Case, Casing};
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::Read;

#[derive(Debug, Deserialize)]
pub struct Action {
    pub name: String,
    pub variant: Option<String>,
}

#[derive(Debug, Deserialize)]
pub struct Transition {
    pub event: String,
    pub next: String,
    pub actions: Option<Vec<Action>>,
    pub guards: Option<Vec<GuardCondition>>,
}

#[derive(Debug, Deserialize)]
pub struct GuardCondition {
    pub name: String,
    pub condition: bool,
}

#[derive(Debug, Clone)]
pub enum InitialSpec<'a> {
    Always,
    OnlyIf(&'a Vec<GuardCondition>),
}

#[derive(Debug, Clone)]
pub enum StateType<'a> {
    Hierarchical {
        states: &'a Vec<State>,
        exit: &'a String,
    },
    Leaf(&'a Vec<Transition>),
}

#[derive(Debug, Deserialize)]
pub struct State {
    pub name: String,
    #[serde(skip)]
    pub namespaced_name: String,
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
            StateType::Leaf(transitions) => {
                for t in transitions {
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
    // const NONE: Self = Self { name: String::new(),  };
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

#[derive(Debug, Deserialize)]
pub struct Hsm {
    pub states: Vec<State>,
}

impl Hsm {
    pub fn from_yaml_file(file_path: &str) -> Result<Hsm, HsmParserError> {
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
    fn guards(&self) -> HashSet<String> {
        let mut guards = HashSet::new();
        for state in &self.states {
            for g in state.collect_guards() {
                guards.insert(g.clone());
            }
        }
        guards
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
        if state.namespaced_name == "" {
            state.namespaced_name = state.name.clone();
        }
        if let Some(sub_states) = &mut state.states {
            for sub_state in sub_states {
                sub_state.namespaced_name = format!("{}_{}", state.name, sub_state.name);
                Self::resolve_namespaces(sub_state);
            }
        }
    }

    /// Generate a state method as expected by the 'statig' crate.
    fn statig_state_fn(fsm_impl: &mut codegen::Impl, state: &State, superstate: Option<&String>) {
        if let Some(substates) = &state.states {
            let attr: String = if let Some(s) = superstate {
                format!(
                    "superstate(superstate = \"{}\", entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                    s, state.namespaced_name, state.namespaced_name
                )
            } else {
                format!(
                    "superstate(entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                    state.namespaced_name, state.namespaced_name
                )
            };
            fsm_impl
                .new_fn(&state.namespaced_name)
                .attr(&attr)
                .arg_ref_self()
                .arg("event", "&Event")
                .ret("Response<State>");
            for substate in substates {
                Self::statig_state_fn(fsm_impl, substate, Some(&state.name));
            }
        } else {
            let attr: String = if let Some(s) = superstate {
                format!(
                    "state(superstate = \"{}\", entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                    s, state.namespaced_name, state.namespaced_name
                )
            } else {
                format!(
                    "state(entry_action = \"{}_entry\", exit_action = \"{}_exit\")",
                    state.namespaced_name, state.namespaced_name
                )
            };
            fsm_impl
                .new_fn(&state.namespaced_name)
                .attr(&attr)
                .arg_ref_self()
                .arg("event", "&Event")
                .ret("Response<State>");
        }
        let entry_fn = fsm_impl
            .new_fn(&format!("{}_entry", state.namespaced_name))
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
            .new_fn(&format!("{}_exit", state.namespaced_name))
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

    pub fn emit_statig(&self) -> String {
        let mut output = Scope::new();
        output.import("statig::prelude", "*");
        let guards = output
            .new_struct("Guards")
            .derive("Debug")
            .derive("Clone")
            .derive("Copy")
            .derive("PartialEq")
            .derive("Eq");
        for guard in self.guards() {
            guards.field(&guard, "bool");
        }
        let mut actions = codegen::Trait::new("Actions");
        for (action_name, action_type) in self.actions().map.iter() {
            let func = actions.new_fn(action_name);
            if let ActionType::Parametrized { variants } = &action_type {
                let variant_typename = format!("{}_variant", action_name).to_case(Case::UpperCamel);
                func.arg("variant", variant_typename.clone());
                let variant_enum = output.new_enum(variant_typename);
                for var in variants {
                    variant_enum.new_variant(var.to_case(Case::UpperCamel));
                }
            }
        }
        output.push_trait(actions);
        output
            .new_struct("Fsm")
            .derive("Debug")
            .derive("Default")
            .field("guards", "Guards")
            .field("actions", "Actions");
        let fsm_impl = output.new_impl("Fsm").r#macro(&format!(
            "#[state_machine(initial = \"{}\")]",
            self.initial().name
        ));
        for state in &self.states {
            Self::statig_state_fn(fsm_impl, state, None);
        }

        output.to_string()
    }
}
