## Texas Higher Education Opportunity Project (THEOP)
https://opr.princeton.edu/archive/theop/

## Data

### Administrative Data
**Adminstrative** data are split into two groups:  **applications** and **transcripts**.   One of each dataset is provided for 8 Texas colleges, for 16 datasets in total.

#### 1. Application Data
The Application Data consists of freshman application records for seven public and two private Texas universities.  Schools are coded by a two-letter abbreviation in the filename (though we agree not to try and identify "any individuals or schools" ?)

Application Data coverage begins prior to the implementation of the Texas top 10% law (H.B. 588) in 1998. Application Data for the two private institutions is available for the period after implementation of the automatic admission law.

Two schools have data on Enrollees only for certain time periods; otherwise data includes all applicants regardless of enrollment decisions.
| University           | Abbrev | Type    | App: Enroll Only | App: All  |   Trans  |
| -------------------- | ------ | ------- | ---------------- | --------- | --- |
| Texas A&M            | AM     | Public  |                  | 1992-2002 |  1992-2007   |
| Texas A&M Kingsville | AMK    | Public  | 1992-1994        | 1995-2002 | 1992-2004    |
| UT Arlington         | AR     | Public  |                  | 1994-2002 | 1994-2002    |
| UT Austin            | AU     | Public  |                  | 1991-2003 | 1991-2004    |
| UT Pan American      | PA     | Public  |                  | 1995-2002 | 1995-2005    |
| UT San Antonio       | SA     | Public  | 1990-1997        | 1998-2004 | 1990-2004    |
| Southern Methodist   | SM     | Public  |                  | 1998-2005 | 1998-2005   |
| Texas Tech           | TT     | Private |                  | 1995-2003 |     1995-2004|
| Rice                 | RI     | Private |                  | 2000-2004 | 2000-2005    |

Southern Methodist appears to be missing from the provided datasets.

#### Recoded Variables
Some variables have a name that ends with a capital letter, R. This indicates that the variable has been recoded rather than appearing exactly as provided by the universities. Examples include SAT score (satR) and high school class rank in deciles (decileR).

[Confidentiality] Small frequency (less than 20) cells were eliminated by collapsing multiple values into range categories. For example, individual test score values are collapsed into test score ranges.

#### Missing Values
Items with a very large share of missing values are excised from all files. Examples of this include data items pertaining to AP test taking, TOEFL test scores and high school grade point average.

[Confidentiality] Some values are set to missing to preserve the well‐established categories but to hide individual values. This strategy is used infrequently to minimize its impacts on analyses.

#### Variable Availability / Comparibility
There is considerable overlap in information provided across institutions, but not every university provided identical sets of data items. See Application Documentation, page 4 (Table 2)
	
---
#### 2. Transcript Data

Each college transcript registers academic progress toward a degree **for a single enrollee in a single semester,** and specifically, provides hours earned, semester GPA, cumulative GPA, and department and field of major.

Because most enrollees attend college for more than one semester, there are usually multiple college transcripts for each enrollee.

#### Variable Availability / Comparibility
With the exception of **gpahrs** for AM, all variables are available for all nine schools.

#### Missing Data
- UT Arlington: There are 4,473 applicants whose enrollee status is missing. The data does not include transcripts for these applicants.

- Southern Methodist: There are 1,380 enrollees for whom there are no college transcripts. These students were new admits who enrolled in 2005, but had not completed a term at the time the THEOP data collection period ended.

---
#### 3. Data Preparation Notes

- (**DONE**) Create Unique Student ID across all datasets (recommend school abbreviation + studentid)
	- "Student identifiers are not unique across institutions. If building a dataset with application records from more than one institution, creation of an institutionid variable is recommended.  However this School-based ID will be needed to join Application Data with associated Transcript Data."
	  
	- "It is not possible to track transfers within this group of nine universities." (does this mean that all prior transcript information transfers to the 'final university'?)

- Applications: Do we need to handle 'Enrollee-only' data differently than 'All Applicants' ?
  
- Applications: Do we need to subset based on data available by year for complete datasets?
  
- Applications: Do we need to subset based on 1998 law for complete before/after datasets?
  
- Applications: Some universities have different scales / values for any given variable - this may require some consolidation.  "See institution‐specific tabulations for details."
	- ethnic
	- citizenship
	- restype

- Applications: Some variables with schoolwise-sparsity might be candidates for removal (see above)
	- termapp (1/9)  (Keep for AMK, see below)
	- sat_not_recenteredR (4/9)
	- quartile (2/9)
	- hslos (2/9)
	- hscentury (2/9)
	- admit_ut_summer (1/9)
	- admit_prov (4/9)
	- utsa_cap (1/9)
  
- Applications: Merge codes from **termapp** with **termdes** where missing for AMK.
	- "Because the termdes variable is missing for many applications in the Texas A&M Kingsville file, the termapp variable is also provided for this institution. Available only for AMK.
	  
- Applications: **utsa_cap** and **admit_ut_summer** are school-specific provisional/alternative admissions programs.  Might consider keeping if relevant to research questions?

- Applications: College Board "recentered" the SAT scoring scales in 1995, may need to consider and transform pre-1995 data.
	- "In April 1995, the SAT replaced the former 200‐800 verbal and quantitative section test scales with new recentered 200‐800 scales... (this) created new scales with mean test score much closer to 500. (Prior to the recentering, mean verbal and math section scores were below 500.) Both section and composite recentered scores are higher than equivalent scores that are not recentered."
	- able 4 summarizes availability and mean values of un‐recentered and recentered SAT scores for each THEOP institution. (Application Documentation Page 16)

- Applications: **satR,** **actR** and **testscoreR** "Test Score Categories" don't always line up between institutions -- documentation says "impossible" to define non-overlapping categories. (Application Documentation Page 17)
  
- Applications: **gradyear** categories don't always line up between institutions -- documentation says "impossible" to define non-overlapping categories. (Application Documentation Page 20)


---

## Other Background

### H.B. 588 (1998)
The judicial ban on consideration of race and ethnicity in admission decisions applied to all public and private institutions, but the top 10% law, which guaranteed admission to students who graduated in the top 10% of their high school class, only applied to public institutions. All students were required to submit test scores for an application to be considered complete; however, standardized test scores were disregarded for rank‐eligible applicants.

The Application Data shows that small numbers of in‐state, top decile applicants were not admitted after the automatic admission law went into effect (1998). **Because qualified applicants were guaranteed admission provided they submitted a completed application, top 10% applicants not admitted were missing at least one or more requirements, such as test scores, application fees, essays or high school transcripts.** Application fees, essays, and high school transcripts are not included in the application data.

---
## Research Questions
Potential questions we might explore with the Administrative data include:

-   Did students admitted under the TX top decile rule over/underperform their peers academically? (GPA, graduation status, time to graduation, etc.) 
-   Were standardized test scores predictive of college academic performance?
-   Were High School economic status rankings predictive of college academic performance?


