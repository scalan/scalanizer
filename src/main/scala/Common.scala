package scalan.plugin

trait Common {
  /** Converts the name of external type to the name of its wrapper. */
  def wrap(name: String) = name + "Wrapper"
  /** Converts the name of external type to the name of the module which
    * contains a wrapper for the type. */
  def wmod(name: String) = name + "Wrappers"
}
