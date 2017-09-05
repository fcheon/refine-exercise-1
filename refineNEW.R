# 0. Load data into RSudio
# 0a. Write to CSV file
library(dplyr)
library(tidyr)
library(readr)
refine_original <- read_csv("C:/refine_original.csv")

# 1. Clean up brand names
# 1a. transform brand names to - philips, akzo, van houten and unilever
refine_original$company <- gsub(pattern = "^[p|f].*", replacement = "philips", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub(pattern = "^a.*", replacement = "akzo", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub(pattern = "^v.*", replacement = "van houten", refine_original$company, ignore.case = TRUE)
refine_original$company <- gsub(pattern = "^u.*", replacement = "unilever", refine_original$company, ignore.case = TRUE)


# 2. separate product code and number
# 2a. make 2 columns, product code and product number
refine_original <- separate(refine_original,2, into = c("Product code", "Product number"), sep = "-")


# 3. Add product categories
refine_original$product_category <- refine_original$`Product code`

# 3a. p = Smartphone, v=TV, x=Laptop, q=Tablet
# 3b. add a column with the product category for each record.
prodcatfunc <- function(x,y){
  sub(pattern= x, replacement = y, refine_original$product_category)
}
refine_original$product_category <- prodcatfunc("p","Smartphone")
refine_original$product_category <- prodcatfunc("v", "TV")
refine_original$product_category <- prodcatfunc("x","Laptop")
refine_original$product_category <- prodcatfunc("q","Tablet")


# 4. Add full address for geocoding
# 4a. To view the customer's address on the map, addresses need to be in a form that can be easily geocoded. 
# 4b. Add a new column "full_address" that concantenates the 3 address fileds: address, city, country, separated by commas.
refine_original$full_address <- paste(refine_original$address, refine_original$city, refine_original$country, sep = ", ")

# 5. Create a dummy variables for company and product category.
# 5a. Both the company name and product category are categorical variables i.e. they take only a fixed set of values. In order to use them in further analysis you need to create dummy variables. Create dummy binary variables for each of them with the prefix company_ and product_
# 5b. Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever.
refine_original <- mutate(refine_original, company_philips = (company == "philips"))
refine_original <- mutate(refine_original, company_akzo = (company== "akzo"))
refine_original <- mutate(refine_original, company_van_houten = (company == "van houten"))
refine_original <- mutate(refine_original, company_unilever = (company == "unilever"))

# 5c. Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet.
refine_original <- mutate(refine_original, product_smartphone = (product_category == "smartphone"))
refine_original <- mutate(refine_original, product_tv = (product_category == "tv"))
refine_original <- mutate(refine_original, product_laptop = (product_category == "laptop"))
refine_original <- mutate(refine_original, product_tablet = (product_category == "tablet"))

# 6. Submit.
newrefine <- refine_original
write_file(newrefine, path = "C:/refine_clean.csv")

