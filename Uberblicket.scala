import scala.swing.Panel
import java.awt._
import java.awt.geom._
import java.io.FileOutputStream
import Color._
import java.nio.file._
 
import java.awt.Color
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import java.io.IOException
import java.io.File
 
import javax.imageio.ImageIO

import com.itextpdf.text.Document
import com.itextpdf.text.pdf.{ PdfWriter } // , DefaultFontMapper }

/**
 * @author clucas
 */
object Uberblicket {

  val paperSizeInches = (8.5, 11.00) // A4
  val outFaceSize = 2 * 72.0 * 0.98 // inches per 5cm * points per inch * empirical conversion factor (blocks seem slightly smaller than 50mm in practice) 
  val nC = 2
  val rand = new scala.util.Random()

  val allColors = if (nC == 2) { // for 2 colors, RB special-case
    Vector(Color.RED, Color.BLUE, Color.WHITE)
  } else {
    for (i <- 0 until nC) yield {
      getHSBColor(i.toFloat / nC, 1.0f, 1.0f)
    }
  }

  def main(args: Array[String]) = {

    args match {
      case emptyArgs if(emptyArgs.isEmpty) => println("Argument required")
      case write9 if(write9(0)) == "write9" => {
        for (i <- 0 until 9) { saveRandomBlicketToPdf(s"output/tmpBlicket${nC}_${i}.pdf") }
      }
      // e.g., "define 0000"
      case defBlick if(defBlick(0)) == "define" => {
        for (colorDef <- args.tail) {
          val colors = colorDef.split("").map(v => allColors(v.toInt))
          saveDrawToPdf(drawBlicket(_, Some(colors)), s"output/blicketSkin${colorDef}.pdf")
        }
      }
      case defPng if (defPng(0)) == "defpng" => {
        println("writing png")
        // CL: 100 seemed a bit pixel-y. If we want fatter edges, let's change the stroke directly.
        val pix = 300
        for (colorDef <- args.tail) {
          val colors = colorDef.split("").map(v => allColors(v.toInt)).toIndexedSeq
          assert(colors.size == 5,"Need to specify 5 color indices, e.g., 01010")
          saveDrawToPng(drawFace(colors, pix, _), s"output/blicketSkin${colorDef}.png",pix,pix)
        }
      }
      case other:Any => {
        println("Unrecognized argument. Try something like 'define 01111' or 'defpng 12211'.")
      }
    }
  }

  def saveRandomBlicketToPdf(path: String, 
    width: Int = (paperSizeInches._1 * 72).toInt,
    height: Int = (paperSizeInches._2 * 72).toInt) = {
    saveDrawToPdf(drawBlicket(_, None), path)
  }

  def saveDrawToPdf(drawFunc: Function1[Graphics2D, Unit], path: String, width: Int = (paperSizeInches._1 * 72).toInt, height: Int = (paperSizeInches._2 * 72).toInt) = {
    saveToPdf(path, width, height, drawFunc)
  }
  
  def saveDrawToPng(drawFunc: Function1[Graphics2D, Unit], path: String, width: Int = 100, height: Int = 100) = {
    saveToPng(path, width, height, drawFunc)
  }

  def drawFace(colors: IndexedSeq[Color], faceSize: Double, g2: Graphics2D) = {
    assert(colors.size == 5)
    val faceColor = colors(0)
    val circSize = faceSize / 3.5
    val starScale = faceSize*0.3
    /* CL: Where do these come from?
    val phi = 1.61803398875
    val q = 0.22451398829*starScale
    val r = 0.19098300562*starScale
    val s = 0.01823725421*starScale
    val t = 0.07294901687*starScale
    val v = 0.2360679775*starScale
    val h = ((1-q-(2*s))/2)*starScale
    */
    val off = (faceSize * 0.05) //stimulus offset
    
    val backgR = new Rectangle2D.Double(0, 0, faceSize, faceSize)
    faceComponent((backgR, faceColor), g2)
    
    //val faceR = polyPath(Vector((0 + (faceSize * 0.05), 0 + (faceSize * 0.05)), (0 + (faceSize * 0.05), faceSize - (faceSize * 0.05)), (faceSize - (faceSize * 0.05), faceSize - (faceSize * 0.05)), (faceSize - (faceSize * 0.05), 0 + (faceSize * 0.05))))
    //faceComponent((faceR, faceColor), g2)
    
    //Star stimulus for blicket
    // CL: This kind of star has rotational symmetry but no axis-aligned edges. 
    val starR = {
      val oRad = starScale/2.0
      val iRad = starScale/4.0

      // CL: Somewhat arbitrary; the be general, this would probably have to depend on stroke width.
      val center = (faceSize/2.0,1.35*oRad)

      val starOuterDeg: IndexedSeq[Double] = (0 to 4).map(v => (0.20*2)*math.Pi*v.toDouble+math.Pi)
      val starInnerDeg: IndexedSeq[Double]  = starOuterDeg.map(v => v+0.1*2*math.Pi) // offset by half
      val starOuter = starOuterDeg.map(v => (oRad*math.sin(v)+center._1,oRad*math.cos(v)+center._2))
      val starInner = starInnerDeg.map(v => (iRad*math.sin(v)+center._1,iRad*math.cos(v)+center._2))

      val starAll = (for(i <- 0 until starOuter.size) yield {
        IndexedSeq(starOuter(i),starInner(i))
      }).flatten
      polyPath(starAll)
    }

    /*
    val starR_old = {
      val oldVec:IndexedSeq[(Double,Double)] = Vector((faceSize/2, off+s), //point A
      ((faceSize/2)+(v/2), s+h+117), //point F
      ((faceSize*(0.65))-(off/3), s+h+117), //point B
      ((faceSize/2)+(1+v)/2+t, s+h+q+119), //point G
      ((faceSize*(0.65))-r, faceSize*(0.31666)), //point C
      ((faceSize/2), 27), //point H
      ((faceSize*(0.35))+r, faceSize*(0.31666)), //point D
      ((faceSize/2)-(1+v)/2-t, s+h+q+119), //point I
      ((faceSize*(0.35))+(off/3), s+h+117), //point E
      ((faceSize/2)-(v/2), s+h+117)) //point J
      println(s"old star:" + oldVec.mkString(","))
      polyPath(oldVec)
    }*/
    
    faceComponent((starR, colors(1)),g2)
    
    //Circle stimulus for blicket
    val circleR = new Arc2D.Double(faceSize / 2 - (faceSize*(0.266666)/2), faceSize*(0.68333), faceSize*(0.266666), faceSize*(0.266666), 0, 360, Arc2D.CHORD)
    faceComponent((circleR, colors(2)), g2)
        
    //Square stimulus for blicket
    val squareR = polyPath(Vector((off, faceSize*0.3833333),
    (faceSize*0.2833333, faceSize*0.3833333),
    (faceSize*0.2833333, faceSize*0.6166666),
    (off, faceSize*0.6166666)))
    faceComponent((squareR, colors(3)), g2)
    
    //Triangle stimulus for blicket
    val triangleR = polyPath(Vector((faceSize*(2.5/3), faceSize*0.3833333),
    (faceSize-off, faceSize*0.6166666),
    ((faceSize*(0.666667))+off, faceSize*0.6166666)))
    faceComponent((triangleR, colors(4)), g2)
        
    //val tri0 = polyPath(Vector((0 + (faceSize * 0.05), 0 + (faceSize * 0.05)), (0 + (faceSize * 0.05), faceSize / 3.5), (faceSize / 3.5, 0 + (faceSize * 0.05))))
    //faceComponent((tri0, colors(1)), g2)

    //val at1 = AffineTransform.getRotateInstance(math.Pi, faceSize / 2, faceSize / 2)
    //faceComponent((at1.createTransformedShape(tri0), colors(1)), g2)

    //val at2 = AffineTransform.getRotateInstance(math.Pi * 0.5, faceSize / 2, faceSize / 2)
    //faceComponent((at2.createTransformedShape(tri0), colors(1)), g2)

    //val at3 = AffineTransform.getRotateInstance(math.Pi * 1.5, faceSize / 2, faceSize / 2)
    //faceComponent((at3.createTransformedShape(tri0), colors(1)), g2)
    
    //val tri1 = polyPath(Vector((circSize, circSize), (circSize, faceSize / 2), (faceSize / 2, circSize)))
    //faceComponent((tri1, colors(3)), g2)
    
    //val semiT0 = AffineTransform.getRotateInstance(math.Pi * 0.25, faceSize / 3, faceSize / 2)
    //faceComponent((semiT0.createTransformedShape(tri1), colors(4)), g2)

    //val semiC0 = new Arc2D.Double(faceSize / 2 - circSize / 2, faceSize / 2 - circSize / 2, circSize, circSize, 0, 180, Arc2D.CHORD)
    //faceComponent((semiC0, colors(3)), g2)

    //val semiC1 = new Arc2D.Double(faceSize / 2 - circSize / 2, faceSize / 2 - circSize / 2, circSize, circSize, 180, 180, Arc2D.CHORD)
    //faceComponent((semiC1, colors(4)), g2)

    //val directionMarker = new Arc2D.Double((0 + faceSize * 0.05) - circSize / 2, (faceSize - faceSize * 0.05) / 2 - circSize / 2, circSize, circSize, 270, 180, Arc2D.CHORD)
    //sHelp((directionMarker, Color.BLACK), g2, true)

    //sHelp((faceR, Color.BLACK), g2, false)
  }

  private def faceComponent(sc: (Shape, Color), g2: Graphics2D, drawOutline: Boolean = true) = {
    sHelp(sc,g2,true)
    if(drawOutline) sHelp((sc._1,Color.BLACK),g2,false)
  }

  private def sHelp(sc: (Shape, Color), g2: Graphics2D, fill: Boolean = true) = {
    g2.setColor(sc._2)
    if (fill) g2.fill(sc._1) else {
      val tmpStroke = g2.getStroke
      g2.setStroke(new BasicStroke(5));
      g2.draw(sc._1)
      g2.setStroke(tmpStroke)
    }
  }

  def drawMultipleStrips(g2: Graphics2D) = {
    val (baseBoxHeight, baseBoxWidth) = (72, 72)
    val margin = 36
    var offset = margin
    val colors = allColors
    for (heightMod <- Vector(.1, .2, .4, .8); widthMod <- Vector(1.5); reps <- Vector(0)) {
      val boxWidth = (baseBoxWidth * widthMod).toInt
      val boxHeight = (baseBoxHeight * heightMod).toInt
      drawColorStrip(g2, colors, (boxHeight, boxWidth), (offset, margin))
      offset = offset + boxWidth + 15
    }
  }

  def drawColorStrip(g2: Graphics2D, colors: IndexedSeq[Color], boxSize: (Int, Int), offset: (Int, Int)) = {
    val (boxHeight, boxWidth) = boxSize

    println(s"bh: ${boxHeight}, bw: ${boxWidth}")
    var i = 0;
    var maxSize = 72 * 10;
    var start = offset._2
    while (start < maxSize) {
      val color = colors(i % colors.size)
      g2.setColor(color)
      println(start + " " + i)
      g2.fillRect(0 + offset._1, start, boxWidth, boxHeight)
      start = start + boxHeight
      i = i + 1;
    }
  }

  def drawBlicket(g2: Graphics2D, colorsO: Option[IndexedSeq[Color]] = None) = {
    val colors = colorsO.getOrElse(for (i <- 0 until 5) yield allColors(rand.nextInt(allColors.size)))
    val borderSize = 2
    val innerFaceSize = outFaceSize - borderSize
    val wrapCompensation = .0125 // adjusting for the fact that edges have to wrap around the cube
    val padding = (30, 30)
    val faceCoords = Vector((0, 1), (1, 1), (1, 0), (1, 2), (2, 1), (3, 1))
    for (fc <- faceCoords) {
      val translate = AffineTransform.getTranslateInstance(padding._1 + fc._2 * (outFaceSize*(1.0+wrapCompensation)), padding._2 + fc._1 * (outFaceSize*(1.0+wrapCompensation)))
      g2.setTransform(translate)
      sHelp((new Rectangle2D.Double(-borderSize / 2, -borderSize / 2, outFaceSize, outFaceSize), Color.BLACK), g2)
      drawFace(colors, innerFaceSize, g2)
    }

  }

  def polyPath(points: IndexedSeq[(Double, Double)]): Path2D.Double = {
    val pp = new Path2D.Double();
    var isFirst = true;
    for (p <- points) {
      if (isFirst) {
        pp.moveTo(p._1, p._2);
        isFirst = false;
      } else {
        pp.lineTo(p._1, p._2);
      }
    }
    pp.closePath();
    pp
  }
  
  def saveToPng(pathStr: String, width: Int, height: Int, paintF: (java.awt.Graphics2D) => Unit): Unit = {
    println("calling saveToPng")
    val document = new Document()
    try {
      val path = java.nio.file.Paths.get(pathStr)
      println(path.toAbsolutePath())
      
      //Create new BufferedImage to store the output of drawFace (i.e. a single blicket face)
      val bimg = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      val g = bimg.createGraphics()
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, bimg.getWidth, bimg.getHeight)
      g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
	       java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
	  paintF(g)
	  ImageIO.write(bimg, "png", new File(pathStr))
	} catch {
      case ioe: IOException => println(ioe)
    } finally {
	document.close()
	}
  }
		   
  def saveToPdf(pathStr: String, width: Int, height: Int, paintF: (java.awt.Graphics2D) => Unit): Unit = {

    val document = new Document()
    try {
      val path = java.nio.file.Paths.get(pathStr)
      println(path.toAbsolutePath())
      val fos = new FileOutputStream(pathStr)
      val writer = PdfWriter.getInstance(document,fos)
      document.open()
      val contentByte = writer.getDirectContent
      val graphics2d = new com.itextpdf.awt.PdfGraphics2D(contentByte, width, height)
      // The pdf is padded on the top, but drawing the rectangle indicates that's an issue with the third-party library
      //val rectangle2d = new Rectangle2D.Double(0, 0, width, height);
      //graphics2d.draw(rectangle2d)
      paintF(graphics2d)
      graphics2d.dispose();
    } finally {
      document.close()
    }
  }

}