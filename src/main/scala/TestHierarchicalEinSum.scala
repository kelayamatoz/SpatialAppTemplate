import spatial.dsl._

@spatial object TestHierarchicalEinSum extends SpatialApp with CommonParams {
  def main(args: Array[String]): Unit = {
    val x_24_ConCat_DRAM = DRAM[T](i32, i96)
    val x_25_ConCat_DRAM = DRAM[T](i96)
    val x_6_Tensor_L15_DRAM = DRAM[T](i32)
    setMem(x_24_ConCat_DRAM, Matrix.tabulate[T](i32, i96) { (_, _) =>
      0.1.to[T]
    })
    setMem(x_25_ConCat_DRAM, Array.tabulate[T](i96) { _ =>
      0.1.to[T]
    })

    Accel {
      val x_6_cache = SRAM[T](I32(32))
      Foreach(I32(32) by I32(1) par I32(1), I32(96) by I32(32) par I32(1)) {
        (idx0, idx1) =>
          val x_24_cache = SRAM[T](I32(1), I32(32))
          val x_25_cache = SRAM[T](I32(32))

          // TODO: This is an extra step that shall be avoided.
          x_24_cache load x_24_ConCat_DRAM(idx0 :: idx0 + I32(1),
                                           idx1 :: idx1 + I32(32) par dPar)
          x_25_cache load x_25_ConCat_DRAM(idx1 :: idx1 + I32(32) par dPar)
          val accum = Reg[T](0.to[T])
          // TODO: Technically, we could have a two-row input where we want to
          //  do map-reduce instead of reduce.
          val update = Reduce(0.to[T]) (I32(32) by I32(1) par ip) { idx2 =>
            val a = x_24_cache(I32(0), idx2)
            val b = x_25_cache(idx2)
            val c = a * b
            c
          } {_ + _}
          val newAccum = update + accum
          accum := mux(idx1 == I32(0), update, newAccum)
          if (idx1 == I32(64)) {
            x_6_cache(idx0) = accum.value
          }
      }

      x_6_Tensor_L15_DRAM(I32(0) :: I32(32) par dPar) store x_6_cache
    }

    val x_6 = getMem(x_6_Tensor_L15_DRAM)
    printArray(x_6, "x_6 = ")
  }
}
