structure CompVax = Batch(structure M=VaxMO and A=VaxAO)
structure CompM68 = Batch(structure M=M68MO and A=M68AO)
structure IntVax = Interact(VaxMO)
structure IntM68 = Interact(M68MO)
