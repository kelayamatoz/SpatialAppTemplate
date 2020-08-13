import spatial.dsl._

@spatial object TestMergedEinSum extends SpatialApp with CommonParams {
  def main(args: Array[String]): Unit = {

    val x_24_ConCat_DRAM = DRAM[T](i32, i96)
    val x_25_ConCat_DRAM = DRAM[T](i96)
    val x_6_Tensor_L15_DRAM = DRAM[T](i96)
    setMem(x_24_ConCat_DRAM, Matrix.tabulate[T](i32, i96) { (_, _) =>
      0.1.to[T]
    })
    setMem(x_25_ConCat_DRAM, Array.tabulate[T](i96) { _ =>
      0.1.to[T]
    })

    Accel {
      // Conclusion: the meta-programmed version seems less efficient
      //  than the non-meta-programmed one. The if-statement check seems
      //  to introduce quite a bit of overhead.
      val x_6_cache = SRAM[T](i32)
      Foreach (i32 by i1 par i1, i96 by tileSize par i1, tileSize by ip) {
        (idx0, idx1, idx2) =>
          val accum = Reg[T](0.to[T])
          val x_24_cache = SRAM[T](i1, i32)
          val x_25_cache = SRAM[T](i32)

          if (idx2 == i0) {
            x_24_cache load x_24_ConCat_DRAM(idx0 :: idx0 + i1,
              idx1 :: idx1 + i32 par dPar)
            x_25_cache load x_25_ConCat_DRAM(idx1 :: idx1 + i32 par dPar)
          }

          val update = List.tabulate[T](ips) {idx3: scala.Int =>
            val addr = idx2 + I32(idx3)
            val a = x_24_cache(i0, addr)
            val b = x_25_cache(addr)
            val c = a * b
            c
          }.sumTree
          val newAccum = update + accum.value
          accum := mux(idx1 == I32(0) & idx2 == i0, update, newAccum)
          if (idx1 == I32(64) & idx2 == tileSize - ip) {
            x_6_cache(idx0) = accum.value
          }
      }

      x_6_Tensor_L15_DRAM(i0 :: i32 par dPar) store x_6_cache
    }

    val x_6 = getMem(x_6_Tensor_L15_DRAM)
    printArray(x_6, "x_6 = ")
  }
}
