structure RS6000UnixCMB =
    CMBFun (structure TargetMachDepVC = RS6000VisComp
	    val version = "batch (target: rs6000-unix)"
	    val targetosn = "unix")

structure RS6000Compiler = struct
    open RS6000VisComp
    open GenericVC
end
