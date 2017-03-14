fileName = "sales_data.txt"
door_attr_filename = "DoorAttributes.csv"
delim = "\t"
header = c("Account.Name", "Sell.Thru.Location.Id", "Country", "Brand...SAP.Detail"
           , "Product.Category...Detail", "Product.Subcategory...Detail"
           ,"Consumer.Segment...Detail", "Consumer.Group.Extension2...Detail"
           , "Reporting.Period.Fiscal.Year.Week", "Sales.Units", "Sales.Dollars")
place_holders = c("Place.Holder.1", "Place.Holder.2")
geo_place_holders = c("Geo.Place.Holder.1","Geo.Place.Holder.2")
temp_header = c("City","Country","State","Geo.Place.Holder.1","Geo.Place.Holder.2")
matching_column_sales = c("Sell.Thru.Location.Id","Country")
matching_column_door = c("Number","Country")
determine_unique_stores = c("Sell.Thru.Location.Id","Country")
cutoff = 800000
nrows_successcsv = 20000
#xprev = c(1,1,1,1,1,2,2,1,1,1)
#x1prev= 1
#x2prev= 1
#x3prev= 1
#x4prev= 1
#x5prev= 1
#x6prev= 2
#x7prev= 2
#x8prev= 1
#x9prev= 1
#x10prev= 1

