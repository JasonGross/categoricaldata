package net.categoricaldata.util

import java.io.UnsupportedEncodingException
import java.security._
import net.tqft.toolkit.Logging

trait Digest  extends Logging {
 def apply(message: String, algorithm: String = "MD5") = {
    val md: MessageDigest = MessageDigest.getInstance(algorithm)
    val bytes = message.getBytes("CP1252")

    try {
      BigInt(1,md.digest(bytes)).toString(16)
    } catch {
      case a: NoSuchAlgorithmException => error("No Algorithm.", a); ""
      case x: UnsupportedEncodingException => warn("Unsupported Encoding.", x); ""
      case e => warn("Unknown error.", e); ""
    }
  }
}

object Digest extends Digest

object MD5 {
  def apply(message: String) = Digest.apply(message, "MD5")
}

object SHA1 {
  def apply(message: String) = Digest.apply(message, "SHA-1")
}