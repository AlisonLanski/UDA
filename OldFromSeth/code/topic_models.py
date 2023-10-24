# Most of this code comes from here:
# https://www.machinelearningplus.com/nlp/topic-modeling-gensim-python/
# Likely the best treatment of gensim that I've encountered.
from sklearn import datasets, ensemble
diabetes = datasets.load_diabetes()

import arrow
import pyarrow.feather as feather
import pandas as pd

import nltk
nltk.download('vader_lexicon')
from nltk.sentiment.vader import SentimentIntensityAnalyzer

songs = pd.read_feather("~/courses/unstructured/data/allLyricsDF.feather")

sid = SentimentIntensityAnalyzer()

songs['scores'] = songs['lyrics'].apply(
  lambda lyrics: sid.polarity_scores(lyrics)['compound'])
  
score_test = songs['scores'] > .5  

songs[score_test == True]["scores"]

for i in range(0, 5):
  score_test = songs.loc[[i]]['scores'] > .5
  pos_lyrics = songs.loc[[i]][score_test == True]['lyrics']
  print(pos_lyrics)

songs.filter(regex=("Other (text)"))

import pyarrow
pyarrow.feather.write_feather(df = songs, dest = "~/Downloads/amazon_instruments_py.feather")

import gensim
import nltk
# nltk.download('stopwords')
import pandas as pd
import pprint as pprint
import spacy

allLyrics = pd.read_feather("~/courses/unstructured/data/allLyricsDF.feather")

allLyrics.head(10)

stop_words = nltk.corpus.stopwords.words('english')

bigram = gensim.models.Phrases(data_words, min_count=5, threshold=100) # higher threshold fewer phrases.
trigram = gensim.models.Phrases(bigram[data_words], threshold=100)  

# Faster way to get a sentence clubbed as a trigram/bigram
bigram_mod = gensim.models.phrases.Phraser(bigram)
trigram_mod = gensim.models.phrases.Phraser(trigram)

def sent_to_words(sentences):
    for sentence in sentences:
        yield(gensim.utils.simple_preprocess(str(sentence), deacc = True))  
        # yield is like return, but will return sequences

data_words = list(sent_to_words(allLyrics.lyrics))

def remove_stopwords(texts):
    return [[word for word in gensim.utils.simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]

def make_bigrams(texts):
    return [bigram_mod[doc] for doc in texts]

def make_trigrams(texts):
    return [trigram_mod[bigram_mod[doc]] for doc in texts]

# Initialize spacy 'en_core_web_sm' model, keeping only tagger component (for efficiency)
# python3 -m spacy download en

nlp = spacy.load('/usr/local/lib/python3.7/site-packages/en_core_web_sm/en_core_web_sm-3.0.0/', disable=['parser', 'ner'])

def lemmatization(texts, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV']):
    """https://spacy.io/api/annotation"""
    texts_out = []
    for sent in texts:
        doc = nlp(" ".join(sent)) 
        texts_out.append([token.lemma_ for token in doc if token.pos_ in allowed_postags])
    return texts_out
  
data_words_nostops = remove_stopwords(data_words)

# Form Bigrams
data_words_bigrams = make_bigrams(data_words_nostops)

data_lemmatized = lemmatization(data_words_bigrams, allowed_postags=['NOUN', 'ADJ', 'VERB', 'ADV'])

id2word = gensim.corpora.Dictionary(data_lemmatized)

texts = data_lemmatized

corpus = [id2word.doc2bow(text) for text in texts]

lda_model = gensim.models.ldamodel.LdaModel(corpus=corpus,
                                           id2word=id2word,
                                           num_topics=5, 
                                           random_state=100,
                                           update_every=1,
                                           chunksize=100,
                                           passes=10,
                                           alpha='auto',
                                           per_word_topics=True)
                                           
pprint.pprint(lda_model.print_topics())
doc_lda = lda_model[corpus]

lda_model.log_perplexity(corpus)

coherence_model_lda = CoherenceModel(model=lda_model, texts=data_lemmatized, dictionary=id2word, coherence='c_v')
coherence_lda = coherence_model_lda.get_coherence()
