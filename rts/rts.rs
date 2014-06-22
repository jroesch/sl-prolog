struct Machine {
    heap: Heap,
    registers: Vec<Cell>
}

/* Deal with overflow checking and the like later */
impl Machine {
    fn new() -> Machine {
        let heap = Heap::new();
        let registers = Vec::with_capacity(10);
        Machine { heap: heap, registers: registers}
    }

    fn H(&self) -> uint { return self.heap.curr }
    
    fn H_plus(&mut self, inc: uint) { self.heap.curr = self.heap.curr + inc; }

    fn put_structure(&mut self, functor: Functor, register: uint) {
        println!("put_struct");
        let top_addr = self.H();
        println!("set heap 1");
        self.set_heap(top_addr, Struct(top_addr + 1));
        println!("set heap 2");
        self.set_heap(top_addr + 1, Func(functor)); 
        //borrow point suck-yness
        let value = self.get_heap(top_addr);
        self.set_register(register, value);
        self.H_plus(2);
    }
    
    fn set_variable(&mut self, register: uint) {
        let H = self.H();
        self.set_heap(H + 1, Ref(H));
        // There is a bug in the borrow checker, do this instead.
        let cell_at_H = self.get_heap(H);
        self.set_register(H + 1, cell_at_H);
        self.H_plus(1);
    }
    
    fn set_value(&mut self, register: uint) {
        let H = self.H();
        let reg_contents = self.get_register(register);
        self.set_heap(H, reg_contents);
        self.H_plus(1);
    }

    /* should be private */
    fn get_heap(&self, addr: uint) -> Cell {
        return self.heap.memory.get(addr).clone();
    }

    fn set_heap(&mut self, addr: uint, value: Cell) {
        if self.heap.memory.len() <= addr {
            self.heap.memory.reserve(addr + 1);
            *self.heap.memory.get_mut(addr) = value;
        } else {
            *self.heap.memory.get_mut(addr) = value;
        }
    }

    fn set_register(&mut self, register: uint, value: Cell) {
        if self.registers.len() - 1 < register {
           self.registers.reserve(register + 1); 
        } else {
            *self.registers.get_mut(register) = value;
        }
    }

    fn get_register(&self, register: uint) -> Cell {
        return self.registers.get(register).clone();
    }

    fn print_debug(&self) {
        println!("Dumping Machine State:");
        self.heap.print_debug();
    }
}

struct Heap {
    memory: Vec<Cell>,
    size: uint,
    curr: uint
}

impl Heap {
    fn new() -> Heap {
        let vec: Vec<Cell> = Vec::with_capacity(INIT_HEAPSIZE);
        let mut fresh_heap = Heap { memory: vec, size: INIT_HEAPSIZE, curr: 0 };
        fresh_heap.fill(Unalloc);
        return fresh_heap;
    }
    
    // fills the capacity completely, useful for after alloc
    fn fill(&mut self, fill: Cell) { 
        let r = range(0u, self.memory.capacity());
        self.fill_range(fill, r); 
    }
    
    fn fill_capacity(&mut self, fill: Cell) {
        let r = range(self.memory.len(), self.memory.capacity());
        self.fill_range(fill, r); 
    }

    fn fill_range(&mut self, fill: Cell, mut rng: std::iter::Range<uint>) { 
        for i in rng {
            self.memory.push(fill.clone());
        }
    }

    fn print_debug(&self) {
        let mut i = 0;
        for cell in self.memory.iter() {
            println!("{}: {} (Bound: {})", i, cell, !self.is_unbound(i));
            i = i + 1;
        }
    }

    fn is_unbound(&self, cell_k: uint) -> bool {
        match self.memory.get(cell_k) {
            &Ref(k) => k == cell_k,
            _ => false
        }
    }
}

#[deriving(Show, Clone)]
enum Cell {
    Ref(uint),
    Struct(uint),
    Func(Functor),
    Unalloc
}

#[deriving(Show, Clone)]
struct Functor {
    name: Atom,
    arity: uint
}

#[deriving(Show)]
enum Term {
    S(Atom, ~[Term])
}

#[deriving(Show, Clone)]
struct Atom {
    repr: ~str
}

static INIT_HEAPSIZE: uint = 10;

/* simple file format, label table followed by code section */
fn main() {
    println!("Starting up the RTS ...");
    let mut machine = Machine::new();
    let range = range(0, 1);
    println!("Allocated machine ...");
    machine.put_structure(Functor { name: Atom { repr: ~"h" }, arity: 2 }, 3);
    machine.print_debug();
    //allocate_term(atom("p"), 
}

