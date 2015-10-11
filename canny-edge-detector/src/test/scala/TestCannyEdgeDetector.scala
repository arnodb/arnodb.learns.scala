import org.specs2._
import org.specs2.matcher.DataTables

class CannyEdgeDetectorKernelSpec extends Specification with DataTables {

    def is = s2"""
A Kernel instance
  must have a width <= the specified width                       $width
  must contain an array of size equal to its width               $kernelSize
  must contain a diff array of size equal to its width           $diffKernelSize
  must have a short description                                  $toStringValue
A Kernel
  must be computed correctly                                     $mustBeComputed
                                                                 """

    val table = "radius" | "width" | "description" |>
        1f ! 2 ! "Kernel(0.14668746, 0.096218705)" |
        2f ! 12 ! "Kernel(0.038972624, 0.034570705, 0.024127936, ...)" |
        3f ! 42 ! "Kernel(0.017521275, 0.016591392, 0.014087467, ...)"

    val ktable = "radius" | "width" | "expected" |>
        1f ! 2 ! new Kernel(
            Array(0.14668746f, 0.096218705f),
            Array(0f, -0.5578444f)) |
        2f ! 12 ! new Kernel(
            Array(0.038972624f, 0.034570705f, 0.024127936f, 0.013246346f, 0.0057184366f,
                  0.0019402397f, 5.171109E-4f),
            Array(0f, -0.21439362f, -0.29700625f, -0.24156818f, -0.13670567f,
                  -0.056765333f, -0.017708112f)) |
        2f ! 16 ! Kernel(2f, 12) |
        3f ! 42 ! new Kernel(
            Array(0.017521275f, 0.016591392f, 0.014087467f, 0.01072536f, 0.0073217186f,
                 0.0044815387f, 0.0024594755f, 0.0012101664f, 5.338496E-4f, 2.11128E-4f),
            Array(0f, -0.103710234f, -0.1758486f, -0.20031267f, -0.18168315f,
                  -0.138382f, -0.09063602f, -0.051697504f, -0.02587392f, -0.011418003f))

    def mustBeComputed = {
        ktable | {
            (r, w, expected) => {
                val k = Kernel(r, w)
                (k.width must be equalTo(expected.width)) and
                (k.kernel must be equalTo(expected.kernel)) and
                (k.diffKernel must be equalTo(expected.diffKernel))
            }
        }
    }

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
