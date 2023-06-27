from scholarly import scholarly
from collections import Counter
import re
import os
import sys
from argparse import ArgumentParser

print("yes")

parser = ArgumentParser()
parser.add_argument("-s", "--subjectid")
parser.add_argument("-n", "--name")

args = parser.parse_args()

def get_author_from_id(idstring):

	author = scholarly.search_author_id(idstring)

	return author

def get_author_from_name(namestring):

	author = scholarly.search_author(namestring)

	return next(author)

if args.subjectid:

	auth_id = args.subjectid
	author = get_author_from_id(auth_id)

if args.name: 

	auth_id = args.name
	author = get_author_from_name(auth_id)

information = scholarly.fill(author)

cpy = information['cites_per_year']

hind5y = information['hindex5y']
hind = information['hindex']
name = author['name']
scholarid = author['scholar_id']

#The h-index is defined as the maximum value of h such that the given author/journal has published at least h papers that have each been cited at least h times.[5] The index is designed to improve upon simpler measures such as the total number of citations or publications. The index works best when comparing scholars working in the same field, since citation conventions differ widely among different fields.

pubs = information["publications"]

def check_year_exists(biblist):

	try: 
		
		year = biblist['bib']['pub_year']

	except: 

		year = "NA"

	return year


def get_publishing_time_history(pubs):

	pubcounts = list()

	for item in pubs:

		if check_year_exists(item)!="NA":

			pubcounts.append(item['bib']['pub_year'])

	return Counter(pubcounts)


def get_journals(pubs):

	jn_list = list()

	for item in pubs:

		if check_year_exists(item)!="NA":

			journal = re.sub(r'[^a-zA-Z ]', '', item['bib']['citation'])

			year = item['bib']['pub_year']

			pair = (journal.strip(), year)

			jn_list.append(pair)

	d = {}
	for v, k in jn_list:
		d.setdefault(k, []).append(v)

	return d


pubhistory = get_publishing_time_history(pubs)

journalhistory = get_journals(pubs)

pubhistory['hind5y'] = hind5y
pubhistory['hind'] = hind
pubhistory['name'] = name

tmp_path = os.path.join("tmp", scholarid)

os.makedirs(tmp_path, exist_ok=True)


with open(os.path.join(tmp_path, 'journalhistory.csv'), 'w') as f:
    for key in journalhistory.keys():
        f.write("%s, %s\n" % (key, journalhistory[key]))


with open(os.path.join(tmp_path, 'pubhistory.csv'), 'w') as f:
    for key in pubhistory.keys():
        f.write("%s, %s\n" % (key, pubhistory[key]))


with open(os.path.join(tmp_path, 'cpy.csv'), 'w') as f:
    for key in cpy.keys():
        f.write("%s, %s\n" % (key, cpy[key]))


#print(scholarid)















#