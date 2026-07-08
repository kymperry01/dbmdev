# predict_dev example outputs are stable

    Code
      dev_params()
    Output
                          a      Tmin     Tmax         m
      egg       0.000359200  1.754000 35.08000  4.347000
      instar1_2 0.000360000 -2.912200 32.45650 60.509200
      instar3   0.000968800  0.789300 32.04000 14.530000
      instar4   0.000674000 -1.225610 32.04873 15.273340
      prepupa   0.001815330  3.963006 33.04467  4.317786
      pupa      0.000396921  2.417172 32.44556 11.991310

---

    Code
      predict_dev(h1, start_date = "2023-09-05")
    Output
      # A tibble: 6 x 6
          gen stage    start_dev           complete_dev        total_days mean_temp_oC
        <dbl> <chr>    <dttm>              <dttm>                   <dbl>        <dbl>
      1     1 egg      2023-09-05 12:00:00 2023-09-11 10:00:00       5.92         15.2
      2     1 instar1~ 2023-09-11 11:00:00 2023-09-19 11:00:00       8            15.4
      3     1 instar3  2023-09-19 12:00:00 2023-09-22 09:00:00       2.87         17.3
      4     1 instar4  2023-09-22 10:00:00 2023-09-27 09:00:00       4.96         13.7
      5     1 prepupa  2023-09-27 10:00:00 2023-09-28 11:00:00       1.04         18.1
      6     1 pupa     2023-09-28 12:00:00 2023-10-07 11:00:00       8.96         13.8

---

    Code
      predict_dev(h1, start_date = "2023-09-02", start_stage = "instar3", start_dev = 0.5,
        gens = 2)
    Output
      # A tibble: 10 x 6
           gen stage   start_dev           complete_dev        total_days mean_temp_oC
         <dbl> <chr>   <dttm>              <dttm>                   <dbl>        <dbl>
       1     1 instar3 2023-09-02 12:00:00 2023-09-03 15:00:00       1.12         18.5
       2     1 instar4 2023-09-03 16:00:00 2023-09-08 11:00:00       4.79         13.8
       3     1 prepupa 2023-09-08 12:00:00 2023-09-09 20:00:00       1.33         15.6
       4     1 pupa    2023-09-09 21:00:00 2023-09-18 09:00:00       8.5          15.4
       5     2 egg     2023-09-18 10:00:00 2023-09-22 18:00:00       4.33         18.7
       6     2 instar~ 2023-09-22 19:00:00 2023-09-30 13:00:00       7.75         15.3
       7     2 instar3 2023-09-30 14:00:00 2023-10-04 11:00:00       3.88         13.0
       8     2 instar4 2023-10-04 12:00:00 2023-10-09 13:00:00       5.04         12.8
       9     2 prepupa 2023-10-09 14:00:00 2023-10-11 21:00:00       2.29         10.6
      10     2 pupa    2023-10-11 22:00:00 2023-10-18 11:00:00       6.54         19.0

---

    Code
      predict_dev(h1, start_date = "2023-10-01", start_stage = "instar4", gens = 4,
        keep = "gens")
    Output
      # A tibble: 4 x 6
          gen stages   start_dev           complete_dev        total_days mean_temp_oC
        <dbl> <chr>    <dttm>              <dttm>                   <dbl>        <dbl>
      1     1 instar4~ 2023-10-01 12:00:00 2023-10-16 12:00:00       15           14.4
      2     2 egg to ~ 2023-10-16 13:00:00 2023-11-23 09:00:00       37.8         13.2
      3     3 egg to ~ 2023-11-23 10:00:00 2023-12-27 14:00:00       34.2         14.1
      4     4 egg to ~ 2023-12-27 15:00:00 2024-01-26 12:00:00       29.9         15.6

---

    Code
      predict_dev(h1, start_date = "2024-03-01", start_stage = "instar1_2", gens = 5,
        direction = "back", keep = "gens")
    Output
      # A tibble: 5 x 6
          gen stages   complete_dev        start_dev           total_days mean_temp_oC
        <dbl> <chr>    <dttm>              <dttm>                   <dbl>        <dbl>
      1     1 instar1~ 2024-03-01 12:00:00 2024-02-16 17:00:00       13.8         15.1
      2     2 pupa to~ 2024-02-16 16:00:00 2024-01-11 12:00:00       36.2         13.5
      3     3 pupa to~ 2024-01-11 11:00:00 2023-12-13 12:00:00       29.0         15.9
      4     4 pupa to~ 2023-12-13 11:00:00 2023-11-02 21:00:00       40.6         12.5
      5     5 pupa to~ 2023-11-02 20:00:00 2023-10-01 11:00:00       32.4         14.8

