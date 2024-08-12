use std::cell::Cell;


pub type CounterType = u32;
pub struct Counter {
    counter: Cell<CounterType>,
}

impl Counter {
    pub fn new(counter: CounterType) -> Self {
        Self {
            counter: Cell::new(counter)
        }
    }

    pub fn increment(&self) {
        let old_value = self.counter.get();
        self.set(old_value + 1);
    }

    pub fn get(&self) -> CounterType {
        self.counter.get()
    }

    pub fn set(&self, value: CounterType) {
        self.counter.set(value);
    }

    pub fn decrement(&self) {
        let old_value = self.get();
        if old_value > 0 {
            self.set(old_value - 1);
        }
    }
}