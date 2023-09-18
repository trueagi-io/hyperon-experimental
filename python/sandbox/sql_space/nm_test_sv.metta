!(extend-py! sql_space)

!(let $space (new-sql-space)
  (add-atom &self (= (my-sqlspace) $space)))


!(sql.select  "id, gid, name, sort_name, begin_date_year, type" "musicbrainz.artist" "begin_date_year=1977" 3)
;result : [(127482, '164f0d73-1234-4e2c-8743-d77bf2191051', 'Kanye West', 'West, Kanye', 1977, 1), (23366, '29bdc1d0-dc34-4f5d-9c22-8b2d66421f49', 'The Dirty Dozen Brass Band', 'Dirty Dozen Brass Band, The', 1977, 2), (35629, 'd2299099-e617-46dd-b013-ba0b4c72bc63', 'Fabolous', 'Fabolous', 1977, 1)]

!(match (my-sqlspace) (SELECT ColumnNames($name $begin_date_year) FROM musicbrainz.artist WHERE begin_date_year=1977 LIMIT 3) ($name $begin_date_year))
;result [("Kanye West" "1977"), ("The Dirty Dozen Brass Band" "1977"), ("Fabolous" "1977")]


