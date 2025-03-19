README.TXT for “Barbarians at the Store? Private Equity, Products, and Consumers (code & data)”

Notes:

- The data have been scrambled to mask company, product, and UPC identifiers from the Nielsen data base. Original identifiers have been replaced with continuous variables.
- For copyright reasons with the Nielsen data base and for size limitations, the data posted are a subset of the full data base.
- The code below is provided to researchers as a way to check the analyses in the paper. Given the limited nature of the data posted, it does not allow to exactly replicate the results in the paper. 

Instructions:
 
1) Place “PE_Products” folder into the appropriate directory 
2) Open “PE_Products_JFCode.do” code in Stata and edit the path on line 16 of the the code to point to the appropriate directory.
3) Run "PE_Products_JFCode.do"


This code uses several Stata packages that are not built in: eststo, egenmore, unique, winsor, reg2hdfe, corrtex, coefplot, and listtex.  
Before running the code, install these packages from Stata repository by typing “net search reg2hdfe” 
(and similarly for the other packages) in Stata’s command prompt and downloading the appropriate package.

