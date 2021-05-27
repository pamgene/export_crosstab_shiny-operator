# Export Crosstab Shiny_operator for Tercen

##### Description

The `export_crosstab_shiny_operator` is used to export a view in Tercen in the crosstab file format as well as other formats.

##### Usage

Input projection|.
---|---
`row`   | represents the variables (e.g. ID)
`col`   | represents the category (e.g. barcode, Row, Factor1)
`y-axis`| measurement value

Output relations|.
---|---
`Operator view`        | view of the Shiny application

##### Details

The operator is used to export Tercen view data to a file. The file type can be either `tsv` or `xlsx`. The file format can be the crosstab format or a collapsed format (when collapsed columns is enabled). There is a separate option to collapse the rows. The operator allows the export of multiple data layers. Furthermore, the text are in the app shows the dimensions of the input data.

##### See Also

[write_fcs_shiny_operator](https://github.com/tercen/write_fcs_shiny_operator)
