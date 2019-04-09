(*
 * arch/arch.sig: CPU architectures and OS configuration for CM
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *
 * author: Matthias Blume (blume@cs.princeton.edu)
 *)
signature ARCH = sig

    exception  BadConf of string and BadCpu of string and BadOS of string

    eqtype cpu
    type os = SMLofNJ.SysInfo.os_kind
    type conf = { cpu: cpu, os: os }

    val cpuname: cpu -> string
    val cpu: string -> cpu
    val cpusym: cpu -> string

    val osname: os -> string
    val os: string -> os

    val confname: conf -> string
end
