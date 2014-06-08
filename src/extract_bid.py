import json
import re
from pprint import pprint

class businessData:
	def __init__(self):
		self.business = []
		self.addresses = []
		self.bid_to_zip = {}
		self.bid_to_cat = {}
		
	def getBusinesses(self, filename):	
		f = open(filename)
		for line in f:
			data = json.loads(line)
			self.business.append(data)
			#pprint(data)
		f.close()

	def getZipcodes(self):
		count = 0
		count1 = 0
		for b in self.business:
			bid = b['business_id']
			addr = b['full_address']
			mod_addr = re.sub(r"\n"," ",addr)
			if re.match(r'^.*(AZ)$',addr):
				count = count + 1
			else:
				postal_code = re.search(r'.*(\d{5}(\-\d{4})?)$', mod_addr)
				if postal_code:
					self.bid_to_zip[bid] = postal_code.group(1)

	def getCategories(self):
		for b in self.business:
			bid = b['business_id']
			cat = b['categories']
			self.bid_to_cat[bid] = cat

	def writeToFile(self, filename):
		with open(filename,'w') as f1:
			for key,value in self.bid_to_zip.iteritems():
				f1.write(key+"\t"+value+"\n")
		f1.close()

	def writeListsToFile(self, filename1, filename2):
		with open(filename1,'w') as f1, open(filename2,'w') as f2:
			for key, value in self.bid_to_cat.iteritems():
				f1.write(key)
				for item in value:
  					f1.write("\t%s" % item)
  					f2.write("%s\t" % item)
  				f1.write("\n")
  				f2.write("\n")
  		f1.close()
  		f2.close()


biz = businessData()

filepath = 'yelp_academic_dataset_business.json'
biz.getBusinesses(filepath)
biz.getZipcodes()
biz.getCategories()
biz.writeToFile('bid_to_zip.txt')
biz.writeListsToFile('bid_to_cat.txt','categories.txt')