use std::collections::HashMap;


pub struct LocalValueHistory<'e> {
    value_escapes: HashMap<&'e String, bool>
}
