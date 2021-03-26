use std::sync::atomic::{AtomicU32, Ordering};

static UUID_COUNTER: AtomicU32 = AtomicU32::new(0);
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct VarUuid(u32);

impl VarUuid {
    pub fn new() -> VarUuid {
        let next_id = UUID_COUNTER.fetch_add(1, Ordering::SeqCst);
        VarUuid(next_id)
    }

    pub fn index(&self) -> u32 {
        self.0
    }
}
