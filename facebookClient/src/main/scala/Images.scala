package com.anupam.siddhant.facebook

import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO
import java.util._
import java.nio.charset.StandardCharsets
import org.apache.commons.codec.binary.Base64

object Images
{

   def EncodeImage(arr:Array[Byte]) :String = //Encodes the image from byte array to string
    { 
        return Base64.encodeBase64URLSafeString(arr)
    }


   def DecodeImage( str:String) : Array[Byte]  = //Decodes the image from string to byte array
    { 
        return Base64.decodeBase64(str)
    }


   def CreateImage(image:Array[Byte]) : BufferedImage = //Creates BufferImage from byte array
    { 
    var in:InputStream  = new ByteArrayInputStream(image)
    var b :BufferedImage = ImageIO.read(in)
    return b
   
    }

   def ImagesEqual( img1:BufferedImage, img2:BufferedImage ):Boolean = //Checks if two BufferImages are equal
   { 
      if (img1.getWidth() == img2.getWidth() && img1.getHeight() == img2.getHeight()) {
          for (x <- 0 until img1.getWidth()) {
              for (y <- 0 until img1.getHeight()) {
                  if (img1.getRGB(x, y) != img2.getRGB(x, y))
                      return false
              }
          }
      } else {
          return false
      }
      return true
  }



  def ImageToString(image:BufferedImage):String =
  {
        var b :  ByteArrayOutputStream  = new ByteArrayOutputStream()
        ImageIO.write(image, "bmp", b)
        var imageInByte : Array[Byte] = b.toByteArray()
        var encodedimage= EncodeImage(imageInByte)
        encodedimage
  }

  def StringToImage(str:String):BufferedImage =
  {
        var decodedimage= DecodeImage(str)
        var img:BufferedImage=CreateImage(decodedimage)
        img
  }


}