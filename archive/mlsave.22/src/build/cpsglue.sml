structure IntVaxCPS = Interact(VaxMC)
structure CompVaxCPS = Batch(structure M=VaxMC and A=VaxAC)
