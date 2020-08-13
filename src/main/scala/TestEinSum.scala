
import spatial.dsl._

@spatial object TestEinSum extends SpatialApp {
def main(args: Array[String]): Unit = {
type T = FixPt[TRUE, _32, _32]
  val x_6_Tensor_L15_DRAM = DRAM[T](32)
  val x_24_ConCat_DRAM = DRAM[T](32,96)
  val x_25_ConCat_DRAM = DRAM[T](96)
  Accel {
    Foreach (32 by 1 par 1, 96 by 32 par 1, 32 by 16) { (idx0, idx1, idx2) =>
    val acc = Reg[T](0.to[T])
    val x_24_ConCat_SRAM = SRAM[T](32, 32)
    if (idx2 == I32(0)){ x_24_ConCat_SRAM load x_24_ConCat_DRAM(idx0 :: idx0 + 1, idx1 :: idx1 + 32 par 4) }
    val x_25_ConCat_SRAM = SRAM[T](32)
    if (idx2 == I32(0)){ x_25_ConCat_SRAM load x_25_ConCat_DRAM(idx1 :: idx1 + 32 par 4) }
    val x_6_Tensor_L15_SRAM = SRAM[T](32)
    val update = List.tabulate(16){ rOffset => 
      val rIdx = rOffset + idx1
      x_24_ConCat_SRAM(idx0, rIdx) * x_25_ConCat_SRAM(rIdx)
    }.sumTree
    acc := mux(idx2 == I32(0), 0.to[T], acc.value + update)
    if (idx2 == 16) { x_6_Tensor_L15_SRAM(idx0) = acc.value }
    if (idx0 == 31) { x_6_Tensor_L15_DRAM(0 :: 32 par 4) store x_6_Tensor_L15_SRAM }
  }
  val r = getMem(x_6_Tensor_L15_DRAM)
  printArray(r)
}