package com.filippodeluca.jsonpath
package core

trait Matcher[T] {
  def apply(dv: T): T
}
