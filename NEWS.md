# scaledescr 0.2.4

* Added PDF export support to `make_dataframe_to_output()` using the `flextable` engine.
* `make_alpha_table()`: The function only  return only scale name, number of items (N), and alpha value.
* Improved internal file extension handling using `switch` logic for better scalability.
* Updated documentation to clarify dependencies for PDF and Word exports.

# scaledescr 0.2.3

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



