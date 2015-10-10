import org.specs2._
import org.specs2.matcher.DataTables

class CannyEdgeDetectorKernelSpec extends Specification with DataTables {

    def is = s2"""
A Kernel instance
  must have a width <= the specified width                       $width
  must contain an array of size equal to its width               $kernelSize
  must contain a diff array of size equal to its width           $diffKernelSize
  must have a short description                                  $toStringValue
                                                                 """

    val table = "radius" | "width" | "description" |>
        1f ! 2 ! "Kernel(0.14668746, 0.096218705)" |
        2f ! 12 ! "Kernel(0.038972624, 0.034570705, 0.024127936, ...)" |
        3f ! 42 ! "Kernel(0.017521275, 0.016591392, 0.014087467, ...)"

    def width = {
        table | {
            (r, w, d) => {
                Kernel(r, w).width must be_<=(w)
            }
        }
    }
    def kernelSize = {
        table | {
            (r, w, d) => {
                val k = Kernel(r, w)
                k.kernel must have size(k.width)
            }
        }
    }
    def diffKernelSize = {
        table | {
            (r, w, d) => {
                val k = Kernel(r, w)
                k.diffKernel must have size(k.width)
            }
        }
    }
    def toStringValue = {
        table | {
            (r, w, d) => {
                val k = Kernel(r, w)
                k.toString must be equalTo(d)
            }
        }
    }

}
