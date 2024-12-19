import hevs.graphics.FunGraphics
import java.io.File

import java.awt.Color
import java.awt.event.{KeyAdapter, KeyEvent}
import javax.swing.Timer
import hevs.graphics.utils.GraphicsBitmap

import java.awt.image.BufferedImage
import java.awt.Image
import scala.collection.mutable.ArrayBuffer
import java.nio.file.Files
import javax.imageio.ImageIO


object IsometricWorld extends App {

  val gridWidth = 20
  val gridHeight = 20
  val tileSize = 32
  val fg = new FunGraphics(700, 700, "Isometric World")


  val world: Array[Array[Boolean]] = Array.fill(gridWidth, gridHeight)(false)


  var cursorX = 0
  var cursorY = 0


  val textureFiles = Array(
    "/res/block_of_amethyst.png",
    "/res/block_of_coal.png",
    "/res/block_of_copper.png",
    "/res/block_of_copper_oxidized.png",
    "/res/block_of_diamond.png",
    "/res/block_of_emerald.png"

  )


  val textures: ArrayBuffer[GraphicsBitmap] = textureFiles.map(file => new GraphicsBitmap(file)).to(ArrayBuffer)


  var selectedTextureIndex = 0

  def currentTexture: GraphicsBitmap = scaledTextures(selectedTextureIndex)


  def scaleBitmap(original: GraphicsBitmap, width: Int, height: Int, originalPath: String): GraphicsBitmap = {
    val originalImage: BufferedImage = original.getBufferedImage
    val scaledImage: Image = originalImage.getScaledInstance(width, height, Image.SCALE_SMOOTH)

    val bufferedScaledImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g = bufferedScaledImage.createGraphics()
    g.drawImage(scaledImage, 0, 0, null)
    g.dispose()

    val originalName = new File(originalPath).getName.replace(".png", "")
    val relativePath = s"src/res/scaled_texture_$originalName.png"
    val tempFile = new File(relativePath)

    tempFile.getParentFile.mkdirs()
    ImageIO.write(bufferedScaledImage, "png", tempFile)

    val absolutePath = tempFile.getAbsolutePath
    println(s"Created and saved scaled texture: $absolutePath")

    if (!tempFile.exists()) {
      throw new RuntimeException(s"File was not created successfully: $absolutePath")
    }

    new GraphicsBitmap(relativePath)
  }

  val scaledTextures = ArrayBuffer[GraphicsBitmap]()

  for ((texture, filePath) <- textures.zip(textureFiles)) {
    val scaledTexture = scaleBitmap(
      original = texture,
      width = tileSize,
      height = (texture.getHeight * tileSize) / texture.getWidth,
      originalPath = filePath
    )
    scaledTextures += scaledTexture
  }


  val timer = new Timer(1000 / 60, _ => drawWorld())
  timer.start()

  val gridCoordinates = Array.tabulate(gridWidth, gridHeight) { (x, y) =>
    val screenX = (x - y) * tileSize / 2 + fg.getFrameWidth / 2
    val screenY = (x + y) * tileSize / 4 + 50
    (screenX, screenY)
  }

  def drawCube(x: Int, y: Int, texture: GraphicsBitmap): Unit = {
    val screenX = x - texture.getWidth / 2
    val screenY = y - texture.getHeight / 2
    fg.drawPicture(screenX, screenY, texture)
  }

  def drawWorld(): Unit = {
    fg.frontBuffer.synchronized {
      fg.clear(Color.white)

      for (x <- 0 until gridWidth; y <- 0 until gridHeight) {
        val (screenX, screenY) = gridCoordinates(x)(y)

        fg.setColor(Color.lightGray)
        fg.drawLine(screenX, screenY, screenX + tileSize / 2, screenY + tileSize / 4)
        fg.drawLine(screenX, screenY, screenX - tileSize / 2, screenY + tileSize / 4)

        if (world(x)(y)) drawCube(screenX, screenY, scaledTextures(selectedTextureIndex))
      }

      val (cursorScreenX, cursorScreenY) = gridCoordinates(cursorX)(cursorY)
      drawCube(cursorScreenX, cursorScreenY, currentTexture)

      fg.setColor(Color.black)
      fg.drawString( 10, 20,s"Texture Index: $selectedTextureIndex")
    }
  }

  fg.setKeyManager(new KeyAdapter() {
    override def keyPressed(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case KeyEvent.VK_UP    => if (cursorY > 0) cursorY -= 1
        case KeyEvent.VK_DOWN  => if (cursorY < gridHeight - 1) cursorY += 1
        case KeyEvent.VK_LEFT  => if (cursorX > 0) cursorX -= 1
        case KeyEvent.VK_RIGHT => if (cursorX < gridWidth - 1) cursorX += 1
        case KeyEvent.VK_SPACE => world(cursorX)(cursorY) = true
        case KeyEvent.VK_BACK_SPACE => world(cursorX)(cursorY) = false
        case KeyEvent.VK_N =>
          selectedTextureIndex = (selectedTextureIndex + 1) % scaledTextures.length
          println(s"Next texture selected: $selectedTextureIndex")
        case KeyEvent.VK_P => //
          selectedTextureIndex = (selectedTextureIndex - 1 + scaledTextures.length) % scaledTextures.length
          println(s"Previous texture selected: $selectedTextureIndex")
        case _ =>
      }
    }
  })
}
