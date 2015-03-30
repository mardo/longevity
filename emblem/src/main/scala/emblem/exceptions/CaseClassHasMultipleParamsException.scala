package emblem.exceptions

import emblem.TypeKey

/** this exception is thrown when a user tries to generate a extractor for a case class that has multiple
 * parameters. this might be supported in the future, under certain criteria. for instance, we should be able
 * to generate a extractor in the case where all but one of the parameters have default values.
 */
class CaseClassHasMultipleParamsException(key: TypeKey[_])
extends GeneratorException(
  key,
  s"extractor generation for case classes with multiple params currently not supported: $key")
