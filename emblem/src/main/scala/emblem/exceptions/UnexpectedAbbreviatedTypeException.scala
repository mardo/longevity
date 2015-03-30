package emblem.exceptions

import emblem.TypeKey

/** this exception is thrown when the short type supplied to [[extractorFor emblem.extractorFor]] does not
 * match the type of the single parameter of the longhand case class. */
class UnexpectedAbbreviatedTypeException(longKey: TypeKey[_], val shortKey: TypeKey[_])
extends GeneratorException(
  longKey,
  s"could not generate a extractor for case class $longKey with short type $shortKey")
