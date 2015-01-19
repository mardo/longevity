package musette.repo.mongo

import longevity.repo._
import musette.domain._
import musette.repo.WikiPageRepo

class MongoWikiPageRepo(implicit repoPool: RepoPool)
extends MusetteMongoRepo[WikiPage](WikiPageType)
with WikiPageRepo