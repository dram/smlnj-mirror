signature MP =
  sig
      type spin_lock
      val spin_lock: unit  -> spin_lock
      val try_lock : spin_lock -> bool
      val lock : spin_lock -> int
      val unlock: spin_lock -> unit

      val debuglock: spin_lock -> string -> int

      val acquireProc: ('a * (unit -> unit)) -> bool
      exception ReleaseProc
      val releaseProc: unit -> 'a
      val getvar: unit ->  'a
      val setvar: 'a -> unit
  end	

