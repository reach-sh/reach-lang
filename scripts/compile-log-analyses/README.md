Mainly, use go.py:

```
python3 go.py
```

This will write various .csv files in the `out/` folder

Each file groups the numbers by a given time period.
`year.csv`, `month.csv`, `day.csv`, `week.csv`
The name of the file says what the grouping period is.

Each file has the following columns:

* `row_count`
* `cumulative_row_count`
* `unique_users`
* `new_users`
* `cumulative_new_users`

`cumulative_X` is the running total of `X`.
Note that it doesn't make sense to keep a running total of `unique_users`,
but this number does help indicate how active our user base is in a given time period.
"new_users" are the subset of unique users seen in a given time period which are also new (never seen before).
It does make sense to keep a running total of new users,
which, when you sum thru today,
adds up to the total users which we claim to have.

