import csv

class category:
    def __init__(self):
        self.bid_to_cat = {}
        self.categories = ["Active Life", "Arts & Entertainment", "Automotive", "Beauty & Spas", "Education", "Event Planning & Services", "Financial Services", "Food", "Health & Medical", "Home Services", "Hotels & Travel", "Local Flavor", "Local Services", "Mass Media", "Nightlife", "Pets", "Professional Services", "Public Services & Government", "Real Estate", "Religious Organizations", "Restaurants", "Shopping"]
        self.food_bids = [];

    def getMainCategory(self, filename):
        with open(filename) as f:
            for line in csv.reader(f, delimiter = "\t"):
                #print line
                c = [x for i,x in enumerate(self.categories) if x in line]
                if c:
                    self.bid_to_cat[line[0]] = c[0]

    def writeToFile(self, filename):
        with open(filename,'w') as f1:
            for key,value in self.bid_to_cat.iteritems():
                f1.write(key+"\t"+value+"\n")
        f1.close()

    def saveRestaurants(self, filename):
        for key,value in self.bid_to_cat.iteritems():
            if value == "Restaurants" or value == "Food":
                self.food_bids.append(key)
                print key
        with open(filename, 'w') as f:
            for item in self.food_bids:
                f.write("%s\n" % item)
        f.close()


cat = category()
cat.getMainCategory("bid_to_cat.txt")
cat.writeToFile("main_categories.txt")
cat.saveRestaurants("bid_to_food.txt")