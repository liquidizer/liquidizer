package org.liquidizer.lib.ssl

import javax.security.auth.x500.X500Principal

import scala.collection.mutable._
import scala.collection.JavaConversions._

object PrincipalUtils {

    private def parse_name(name: String) = {
        val pattern = """(.+)=(.+)""".r
        val pairs = name.split(",") map {
            case pattern(name, value) => (name, value)
        }
        Map(pairs : _*)
    }

    def name_as_map(principal: X500Principal) = {
        parse_name(principal.getName(X500Principal.RFC2253))
    }

}
