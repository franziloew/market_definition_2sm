# Mining data from news sources
# (http://students.washington.edu/adelak/2017/01/?p=209)

import pickle
from eventregistry import *


# Class NewsDownloader, will download all of the articles it can get from
# the EventRegistry database and then save them to file -
# given a list of source URIs1 (e.g. ['www.stern.de', 'www.spiegel.de'])

class NewsDownloader(object):
    er = EventRegistry(apiKey = "6d5188b6-3c3c-4bff-9958-f717bae914c7")

    def __init__(self, name, source_uris):
        # input
        self.name = name
        self.source_uris = source_uris

        # These 3 functions will be defined below
        self.art_uris = self._get_article_uris()
        self.full_articles = self._get_articles()
        self.save_obj()

    ## Get article URIs

    def _get_article_uris(self):
        results = []
        for uri in self.source_uris:
            # for each source URI in the original list,
            # set up a query using EventRegistry’s QueryArticles() class:
            query = QueryArticles(sourceUri=uri)
            # Only return articles from our source
            query.setRequestedResult(RequestArticlesUriList())
            # only give a list of the article URIs:
            results += NewsDownloader.er.execQuery(query).get("uriList", {}).get("results", [])
        return results

    ## Get articles

    def _get_articles(self, n_arts=200): # Max of 200 articles returned per call
        # assert statement is basically a dummy check to make sure the previous step succeeded
        assert self.art_uris is not None

        art_uri_list = self.art_uris
        results = []
        # Take the self.art_uris list and break it into smaller chunks of 200 articles each
        for i in range(0, len(art_uri_list), n_arts):
            small_uri_list = art_uri_list[i:i + n_arts]
            q = QueryArticles.initWithArticleUriList(small_uri_list)
            q.addRequestedResult(
                RequestArticlesInfo(
                    count=n_arts,
                    returnInfo=ReturnInfo(
                        articleInfo=ArticleInfoFlags(bodyLen=-1, socialScore = True)
                    )))
            results += NewsDownloader.er.execQuery(q)['articles']['results']
        return results

    ## Save object

    def save_obj(self, obj=None):
        if obj is None:
            assert self.full_articles is not None
            obj = self.full_articles
        with open(''.join([self.name, '.pkl']), 'wb') as f:
            pickle.dump(obj, f, pickle.HIGHEST_PROTOCOL)
