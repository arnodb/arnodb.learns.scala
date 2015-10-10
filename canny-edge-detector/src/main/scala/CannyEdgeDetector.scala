import scala.annotation.tailrec
import scala.util.control.Breaks._

import java.io.File
import java.net.URL

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

/*
 * Scala implementation of http://www.tomgibara.com/computer-vision/CannyEdgeDetector.java
 */

class Kernel(val kernel: Array[Float], val diffKernel: Array[Float], val width: Int) {

    override def toString = {
        val str = new StringBuilder("Kernel(")
        @tailrec def more(i: Int, prefix: String) : Unit = {
            if (i < kernel.size) {
                str ++= prefix ++= kernel(i).toString
                if (str.length < 42)
                    more(i + 1, ", ")
                else
                    str ++= ", ..."
            }
        }
        more(0, "")
        str += ')'
        str.toString
    }

}

object Kernel {

    val GAUSSIAN_CUT_OFF = 0.005f

    def gaussian(x: Float, sigma: Float): Float = {
        Math.exp(-(x * x) / (2f * sigma * sigma)).toFloat
    }

    def apply(kernelRadius: Float, kernelWidth: Int) = {
        val kernel = Array.fill[Float](kernelWidth)(0)
        val diffKernel = Array.fill[Float](kernelWidth)(0)
        var kwidth = 0
        breakable {
        while (kwidth < kernelWidth) {
            val g1 = gaussian(kwidth, kernelRadius)
            if (g1 <= GAUSSIAN_CUT_OFF && kwidth >= 2)
                break
            val g2 = gaussian(kwidth - 0.5f, kernelRadius)
            val g3 = gaussian(kwidth + 0.5f, kernelRadius)
            kernel(kwidth) = (g1 + g2 + g3) / 3f / (2f * math.Pi.toFloat * kernelRadius * kernelRadius)
            diffKernel(kwidth) = g3 - g2
            kwidth += 1
        }
        }
        new Kernel(kernel.slice(0, kwidth), diffKernel.slice(0, kwidth), kwidth)
    }

}

object CannyEdgeDetector {
    val MAGNITUDE_SCALE = 100f
    val MAGNITUDE_LIMIT = 1000f
    val MAGNITUDE_MAX: Int = (MAGNITUDE_SCALE * MAGNITUDE_LIMIT).toInt
}

class CannyEdgeDetector(
        val lowThreshold: Float = 2.5f,
        val highThreshold: Float = 7.5f,
        val gaussianKernelRadius: Float = 2f,
        val gaussianKernelWidth: Int = 16,
        val contrastNormalized: Boolean = false
    ) {

    var height = 0
    var width = 0
    var picsize = 0
    var data: Array[Int] = null
    var magnitude: Array[Int] = null
    var sourceImage: BufferedImage = null
    var edgesImage: BufferedImage = null

    var xConv: Array[Float] = null
    var yConv: Array[Float] = null
    var xGradient: Array[Float] = null
    var yGradient: Array[Float] = null

    def process = {
        width = sourceImage.getWidth
        height = sourceImage.getHeight
        picsize = width * height
        initArrays
        readLuminance
        if (contrastNormalized)
            normalizeContrast
        computeGradients(gaussianKernelRadius, gaussianKernelWidth)
        val low = Math.round(lowThreshold * CannyEdgeDetector.MAGNITUDE_SCALE)
        val high = Math.round(highThreshold * CannyEdgeDetector.MAGNITUDE_SCALE)
        performHysteresis(low, high)
        thresholdEdges
        writeEdges(data)
    }

    def initArrays = {
        if (data == null || picsize != data.length) {
            data = Array.fill[Int](picsize)(0)
            magnitude = Array.fill[Int](picsize)(0)

            xConv = Array.fill[Float](picsize)(0)
            yConv = Array.fill[Float](picsize)(0)
            xGradient = Array.fill[Float](picsize)(0)
            yGradient = Array.fill[Float](picsize)(0)
        }
    }

    def computeGradients(kernelRadius: Float, kernelWidth: Int) = {

        val kern = Kernel(kernelRadius, kernelWidth)

        var initX = kern.width - 1
        var maxX = width - (kern.width - 1)
        var initY = width * (kern.width - 1)
        var maxY = width * (height - (kern.width - 1))

        var x: Int = initX
        while (x < maxX) {
            var y = initY
            while (y < maxY) {
                val index = x + y
                var sumX = data(index) * kern.kernel(0)
                var sumY = sumX
                var xOffset = 1
                var yOffset = width
                while (xOffset < kern.width) {
                    sumY += kern.kernel(xOffset) * (data(index - yOffset) + data(index + yOffset))
                    sumX += kern.kernel(xOffset) * (data(index - xOffset) + data(index + xOffset))
                    yOffset += width
                    xOffset += 1
                }

                yConv(index) = sumY
                xConv(index) = sumX
                y += width
            }
            x += 1
        }

        x = initX
        while (x < maxX) {
            var y = initY
            while (y < maxY) {
                var sum = 0f
                val index = x + y
                var i = 1
                while (i < kern.width) {
                    sum += kern.diffKernel(i) * (yConv(index - i) - yConv(index + i))
                    i += 1
                }

                xGradient(index) = sum
                y += width
            }
            x += 1
        }

        x = kern.width
        while(x < width - kern.width) {
            var y = initY
            while (y < maxY) {
                var sum = 0f
                val index = x + y
                var yOffset = width
                var i = 1
                while (i < kern.width) {
                    sum += kern.diffKernel(i) * (xConv(index - yOffset) - xConv(index + yOffset))
                    yOffset = width
                    i += 1
                }

                yGradient(index) = sum
                y += width
            }
            x += 1
        }

        initX = kern.width
        maxX = width - kern.width
        initY = width * kern.width
        maxY = width * (height - kern.width)

        x = initX
        while (x < maxX) {
            var y = initY
            while (y < maxY) {
                val index = x + y
                val indexN = index - width
                val indexS = index + width
                val indexW = index - 1
                val indexE = index + 1
                val indexNW = indexN - 1
                val indexNE = indexN + 1
                val indexSW = indexS - 1
                val indexSE = indexS + 1

                val xGrad = xGradient(index)
                val yGrad = yGradient(index)
                val gradMag = hypot(xGrad, yGrad)

                //perform non-maximal supression
                val nMag = hypot(xGradient(indexN), yGradient(indexN))
                val sMag = hypot(xGradient(indexS), yGradient(indexS))
                val wMag = hypot(xGradient(indexW), yGradient(indexW))
                val eMag = hypot(xGradient(indexE), yGradient(indexE))
                val neMag = hypot(xGradient(indexNE), yGradient(indexNE))
                val seMag = hypot(xGradient(indexSE), yGradient(indexSE))
                val swMag = hypot(xGradient(indexSW), yGradient(indexSW))
                val nwMag = hypot(xGradient(indexNW), yGradient(indexNW))
                var tmp = 0f
                /*
                 * An explanation of what's happening here, for those who want
                 * to understand the source: This performs the "non-maximal
                 * supression" phase of the Canny edge detection in which we
                 * need to compare the gradient magnitude to that in the
                 * direction of the gradient; only if the value is a local
                 * maximum do we consider the point as an edge candidate.
                 * 
                 * We need to break the comparison into a number of different
                 * cases depending on the gradient direction so that the
                 * appropriate values can be used. To avoid computing the
                 * gradient direction, we use two simple comparisons: first we
                 * check that the partial derivatives have the same sign (1)
                 * and then we check which is larger (2). As a consequence, we
                 * have reduced the problem to one of four identical cases that
                 * each test the central gradient magnitude against the values at
                 * two points with 'identical support'; what this means is that
                 * the geometry required to accurately interpolate the magnitude
                 * of gradient function at those points has an identical
                 * geometry (upto right-angled-rotation/reflection).
                 * 
                 * When comparing the central gradient to the two interpolated
                 * values, we avoid performing any divisions by multiplying both
                 * sides of each inequality by the greater of the two partial
                 * derivatives. The common comparand is stored in a temporary
                 * variable (3) and reused in the mirror case (4).
                 * 
                 */
                var test: Boolean = false
                if (xGrad * yGrad <= 0f /*(1)*/) {
                    if (Math.abs(xGrad) >= Math.abs(yGrad) /*(2)*/) {
                        tmp = Math.abs(xGrad * gradMag)
                        test = tmp >= Math.abs(yGrad * neMag - (xGrad + yGrad) * eMag) /*(3)*/ && tmp > Math.abs(yGrad * swMag - (xGrad + yGrad) * wMag) /*(4)*/
                    } else {
                        tmp = Math.abs(yGrad * gradMag)
                        test = tmp >= Math.abs(xGrad * neMag - (yGrad + xGrad) * nMag) /*(3)*/ && tmp > Math.abs(xGrad * swMag - (yGrad + xGrad) * sMag) /*(4)*/
                    }
                } else {
                    if (Math.abs(xGrad) >= Math.abs(yGrad) /*(2)*/) {
                        tmp = Math.abs(xGrad * gradMag)
                        test = tmp >= Math.abs(yGrad * seMag + (xGrad - yGrad) * eMag) /*(3)*/ && tmp > Math.abs(yGrad * nwMag + (xGrad - yGrad) * wMag) /*(4)*/
                    } else {
                        tmp = Math.abs(yGrad * gradMag)
                        test = tmp >= Math.abs(xGrad * seMag + (yGrad - xGrad) * sMag) /*(3)*/ && tmp > Math.abs(xGrad * nwMag + (yGrad - xGrad) * nMag) /*(4)*/
                    }
                }
                if (test) {
                    magnitude(index) = if (gradMag >= CannyEdgeDetector.MAGNITUDE_LIMIT) CannyEdgeDetector.MAGNITUDE_MAX else (CannyEdgeDetector.MAGNITUDE_SCALE * gradMag).toInt
                    //NOTE: The orientation of the edge is not employed by this
                    //implementation. It is a simple matter to compute it at
                    //this point as: Math.atan2(yGrad, xGrad);
                } else {
                    magnitude(index) = 0
                }
                y += width
            }
            x += 1
        }
    }

    def hypot(x: Float, y: Float): Float = {
        Math.hypot(x, y).toFloat
    }

    def performHysteresis(low: Int, high: Int) {
        //NOTE: this implementation reuses the data array to store both
        //luminance data from the image, and edge intensity from the processing.
        //This is done for memory efficiency, other implementations may wish
        //to separate these functions.
        data = Array.fill[Int](picsize)(0)

        var offset = 0
        var y = 0
        while (y < height) {
            var x = 0
            while (x < width) {
                if (data(offset) == 0 && magnitude(offset) >= high) {
                    follow(x, y, offset, low)
                }
                offset += 1
                x += 1
            }
            y += 1
        }
     }

    def follow(x1: Int, y1: Int, i1: Int, threshold: Int) {
        val x0 = if (x1 == 0) x1 else x1 - 1
        val x2 = if (x1 == width - 1) x1 else x1 + 1
        val y0 = if (y1 == 0) y1 else y1 - 1
        val y2 = if (y1 == height -1) y1 else y1 + 1

        data(i1) = magnitude(i1)
        var x = x0
        while (x <= x2) {
            var y = y0
            while (y <= y2) {
                val i2 = x + y * width
                if ((y != y1 || x != x1)
                    && data(i2) == 0
                    && magnitude(i2) >= threshold) {
                    follow(x, y, i2, threshold)
                    return
                }
                y += 1
            }
            x += 1
        }
    }

    def thresholdEdges() {
        var i = 0
        while (i < picsize) {
            data(i) = if (data(i) > 0) -1 else 0xff000000
            i += 1
        }
    }

    def luminance(r: Float, g: Float, b: Float): Int = {
        Math.round(0.299f * r + 0.587f * g + 0.114f * b)
    }

    def readLuminance() {
        val type_ = sourceImage.getType()
        if (type_ == BufferedImage.TYPE_INT_RGB || type_ == BufferedImage.TYPE_INT_ARGB) {
            val pixels = sourceImage.getData().getDataElements(0, 0, width, height, null).asInstanceOf[Array[Int]]
            var i = 0
            while (i < picsize) {
                val p = pixels(i)
                val r = (p & 0xff0000) >> 16
                val g = (p & 0xff00) >> 8
                val b = p & 0xff
                data(i) = luminance(r, g, b)
                i += 1
            }
        } else if (type_ == BufferedImage.TYPE_BYTE_GRAY) {
            val pixels = sourceImage.getData().getDataElements(0, 0, width, height, null).asInstanceOf[Array[Byte]]
            var i = 0
            while (i < picsize) {
                data(i) = (pixels(i) & 0xff)
            }
        } else if (type_ == BufferedImage.TYPE_USHORT_GRAY) {
            val pixels = sourceImage.getData().getDataElements(0, 0, width, height, null).asInstanceOf[Array[Short]]
            var i = 0
            while (i < picsize) {
                data(i) = (pixels(i) & 0xffff) / 256
                i += 1
            }
        } else if (type_ == BufferedImage.TYPE_3BYTE_BGR) {
            val pixels = sourceImage.getData().getDataElements(0, 0, width, height, null).asInstanceOf[Array[Byte]]
            var offset = 0
            var i = 0
            while (i < picsize) {
                val b = pixels(offset) & 0xff; offset += 1
                val g = pixels(offset) & 0xff; offset += 1
                val r = pixels(offset) & 0xff; offset += 1
                data(i) = luminance(r, g, b)
                i += 1
            }
        } else {
            throw new IllegalArgumentException("Unsupported image type: " + type_)
        }
    }

    def normalizeContrast() {
        val histogram = Array[Int](256)
        var i = 0
        while (i < data.length) {
            histogram(data(i)) += 1
            i += 1
        }
        val remap = Array[Int](256)
        var sum = 0
        var j = 0
        i = 0
        while (i < histogram.length) {
            sum += histogram(i)
            val target = sum*255/picsize
            var k = j + 1
            while (k <= target) {
                remap(k) = i
                k += 1
            }
            j = target
            i += 1
        }

        i = 0
        while (i < data.length) {
            data(i) = remap(data(i))
            i += 1
        }
    }

    def writeEdges(pixels: Array[Int]) {
        //NOTE: There is currently no mechanism for obtaining the edge data
        //in any other format other than an INT_ARGB type BufferedImage.
        //This may be easily remedied by providing alternative accessors.
        if (edgesImage == null) {
            edgesImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
        }
        edgesImage.getWritableTile(0, 0).setDataElements(0, 0, width, height, pixels)
    }

}

object CommandLine {
    def main(args: Array[String]): Unit = {
        val image = ImageIO.read(new URL(args(0)))
        val detector = new CannyEdgeDetector()
        detector.sourceImage = image
        detector.process
        val edges = detector.edgesImage
        val out = new File(args(1))
        ImageIO.write(edges, "png", out)
    }
}
