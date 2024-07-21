use serde::{Serialize, Deserialize};
mod hsm;
/// State specification
#[derive(Debug, Serialize, Deserialize)]
pub struct State {
    /// State's name
    pub name: String,
    /// State's description (optional)
    pub description: Option<String>,
    /// Sub-states in case hierachical design (optional)
    pub states: Vec<State>,
    /// Transitions from this state, including self-transitions.
    pub transitions: Vec<Transition>,
    /// Action to execute when entering this state.
    pub entry_action: Option<Action>,
    /// Action to execute when exiting this state.
    pub exit_action: Option<Action>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Action {
    pub name: String,
    pub variants: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ActionVariant {
    pub action: Action,
    pub variant: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GuardCondition {
    pub name: String,
    pub cond: bool,
}

/// Transition specification
#[derive(Debug, Serialize, Deserialize)]
pub struct Transition {
    /// Event that triggers this transition. 
    pub event: Event,
    /// The state this transition leads to. 
    pub next_state: State,
    /// Optional action to perform on this transition.
    pub action: Option<ActionVariant>,
    /// Optional description
    pub description: Option<String>,
    /// Optional transition guard.
    /// The transition does only happen if this guard condition is met.
    pub guard: Option<GuardCondition>,
}

/// Event specification
#[derive(Debug, Serialize, Deserialize)]
pub struct Event {
    pub name: String,
    pub description: Option<String>
}

impl State {
    /// Return events that can be processed by this state or any sub-state.
    pub fn events(&self) -> Vec<Event> {
        vec![]
    }
    
    pub fn is_superstate(&self) -> bool {
        !self.states.is_empty()
    }

    pub fn guards(&self) -> Vec<&str> { 
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let hsm = hsm::Hsm::from_yaml_file("test.yaml").expect("YAML deser failed");
        println!("{:?}", hsm);
        let statig = hsm.emit_statig();
        println!("{}", statig);
    }
}
