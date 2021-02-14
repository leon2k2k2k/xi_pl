use std::sync::atomic::{AtomicU32, Ordering};

static FREE_VAR_COUNTER: AtomicU32 = AtomicU32::new(std::u32::MAX);

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct FreeVar(pub u32);

impl FreeVar {
    pub fn new() -> FreeVar {
        let next_id = FREE_VAR_COUNTER.fetch_add(std::u32::MAX, Ordering::SeqCst);
        FreeVar(next_id)
    }
}
