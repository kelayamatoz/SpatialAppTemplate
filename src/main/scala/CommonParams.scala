import spatial.{dsl => d}

trait CommonParams {
  type T = d.FixPt[d.TRUE, d._16, d._16]
  val tileSize = d.I32(32)
  val dPar = d.I32(4)
  val step = d.I32(1)
  val baseAddr = d.I32(0)
  val i32 = d.I32(32)
  val i96 = d.I32(96)
  val i1 = d.I32(1)
  val i0 = d.I32(0)
  val ip = d.I32(16)
  val ips = 16

}
