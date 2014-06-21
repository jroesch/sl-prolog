//type Heap = Vec<Cell>;

struct Heap {
    memory: Vec<Cell>,
    size: uint
}

impl Heap {
    fn new() -> Heap {
        let vec: Vec<Cell> = Vec::with_capacity(INIT_HEAPSIZE);
        let mut fresh_heap = Heap { memory: vec, size: INIT_HEAPSIZE };
        fresh_heap.fill();
        return fresh_heap;
    }

    fn fill(&mut self) { 
        for i in range(0u, self.size) {
            self.memory.push(Ref(i));
        }
        println!("Initial Heap Size: {}", self.size);
    }

    fn debug_print(&self) {
        let mut i = 0;
        for cell in self.memory.iter() {
            println!("{}: {}", i, cell);
            i = i + 1;
        }
    }
}

#[deriving(Show)]
enum Cell {
    Ref(uint),
}

static INIT_HEAPSIZE: uint = 10;

fn main() {
    println!("Starting up the RTS ...")
    let heap = Heap::new(); 
    heap.debug_print();
}

