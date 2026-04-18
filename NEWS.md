# scaledescr 0.2.4
* number of function-13

## Improvements
* Added PDF export support to `make_dataframe_to_output()` using the `flextable` engine.
* `make_alpha_table()`: The function only  return only scale name, number of items (N), and alpha value.
* Improved internal file extension handling using `switch` logic for better scalability.
* Updated documentation to clarify dependencies for PDF and Word exports.


# scaledescr 0.2.3
* number of function-13

## Improvements
* `make_scale_description_table()`: now  accepts multiple columns at once to generate a single descriptive table,and Accepts either a numeric vector (single column) or a data frame with column names.
* `make_alpha_table()`: users are warned that extraction of CI values may be unreliable across psych::alpha versions. Future versions will return only scale name, number of items (N), and alpha value.
* `make_chisq_test_table()`:now accept list of chi square test and return single frame with rows of all chi square test result extracted. 

## New Functions
* `make_alphanumeric_conversion()` - Converts specified columns between alphabetical (text) and numeric values. Can either overwrite existing columns or create new ones.   
* `make_scale_total()` - Converts specified columns between alphabetical (text) and numeric values.Can either overwrite existing columns or create new ones.   
* `make_reverse_score()` - Create Reverse Scores as New Columns.   
* `compute_ICC()` - Create Reverse Scores as New Columns.   
* `make_one_sample_t_test_table()` -  create a single row table of one sample t test result from htest object calculated using t.test(). it can accept list of htest object at once as well.


# scaledescr 0.2.2
* number of function-8

## Improvements
* `make_dataframe_to_output()`: now trims whitespace and standardizes case in character variables so that values such as "student", " Student ", and "STUDENT" are treated as the same category.

## New Export Functions
* `make_demographic_table_to_output()` - Generates a demographic summary table using `make_demographic_table()` and exports the resulting table to Word, Excel, or CSV format.


# scaledescr 0.2.1
* number of function-7

## Improvements
* `make_dataframe_to_output()` - fixed to handle CSV, Word, and Excel exports properly.
* Updated format argument: `"docs"` replaced with `"word"` for Word output.


# scaledescr 0.2.0

## New Functions
* `make_chisq_test_table()`
* `make_independent_t_test_table()`


# scaledescr 0.1.3
* Initial CRAN release of the package 
* number of function-5

## List of functions
*`make_alpha_table()`
*`make_dataframe_to_output()`
*`make_demographic_table()`
*`make_paired_t_test_table()`
*`make_scale_description_table()`
