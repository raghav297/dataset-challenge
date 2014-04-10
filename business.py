import json
import re
from pprint import pprint

class businessData:
	def __init__(self):
		self.business = []
		self.addresses = []
		self.bid_to_zip = {}
		
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

	def writeZipToFile(self, filename):
		with open(filename,'w') as f1:
			for key,value in self.bid_to_zip.iteritems():
				f1.write(key+"\t"+value+"\n")
		f1.close()


biz = businessData()

filepath = 'yelp_phoenix_academic_dataset/yelp_academic_dataset_business.json'
biz.getBusinesses(filepath)
biz.getZipcodes()
biz.writeZipToFile('bid_to_zip.txt')