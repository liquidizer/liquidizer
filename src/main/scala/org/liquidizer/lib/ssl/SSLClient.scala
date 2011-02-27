package org.liquidizer.lib.ssl

import java.security.cert.X509Certificate

import net.liftweb._
import common.{Full, Empty}
import http.S
import http.provider.servlet.HTTPRequestServlet

object SSLClient {
    private def attribute[T](attr: String) = {
        S.containerRequest flatMap { request =>
            val a = request.asInstanceOf[HTTPRequestServlet].req.
                getAttribute(attr).asInstanceOf[T]
            if (a != null) Full(a) else Empty
        }
    }

    private def valid(cert: X509Certificate) = {
        try {
            cert.checkValidity
            true
        } catch {
            case _ => false
        }
    }



    def certificates =
        attribute[Array[X509Certificate]]("javax.servlet.request.X509Certificate")

    def valid_certificates =
        certificates map { _.filter {valid(_)} }

    def key_size = attribute[Int]("javax.servlet.request.key_size")

    def cipher_suite = attribute[String]("javax.servlet.request.cipher_suite")

    def certificate_id(cert: X509Certificate) =
        cert.getIssuerX500Principal.getName + cert.getSerialNumber.toString(16)

}
