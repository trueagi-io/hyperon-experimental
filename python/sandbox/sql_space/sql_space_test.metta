!(extend-py! sql_space)
; database can be installed with use of   https://github.com/metabrainz/musicbrainz-docker#publish-ports-of-all-services
; the description https://musicbrainz.org/doc/MusicBrainz_Database
!(bind! &sql_space (new-sql-space musicbrainz_db localhost musicbrainz musicbrainz 5432))
;!(sql.save-query-result &sql_space &self(, (musicbrainz.artist (id $id) (name $name)) (musicbrainz.artist (id $id) (begin_date_year 1977)) (limit 3)))
;!(get-atoms &self)
;result : [(127482, '164f0d73-1234-4e2c-8743-d77bf2191051', 'Kanye West', 'West, Kanye', 1977, 1), (23366, '29bdc1d0-dc34-4f5d-9c22-8b2d66421f49', 'The Dirty Dozen Brass Band', 'Dirty Dozen Brass Band, The', 1977, 2), (35629, 'd2299099-e617-46dd-b013-ba0b4c72bc63', 'Fabolous', 'Fabolous', 1977, 1)]

;!(match &sql_space (SELECT ColumnNames($name $begin_date_year) FROM musicbrainz.artist WHERE begin_date_year=1977 LIMIT 3) ($name $begin_date_year))


;!(match &sql_space (, (musicbrainz.artist (id $id) (name $name)) (musicbrainz.artist (id $id) (begin_date_year 1983)) (limit 3)) $name)
;result [("Kanye West" "1977"), ("The Dirty Dozen Brass Band" "1977"), ("Fabolous" "1977")]
;!(sql.insert &sql_space "musicbrainz.artist"
; "gid, name, sort_name, begin_date_year, begin_date_month, begin_date_day, type, area, gender" "uuid_generate_v4(),'some name', 'name some', 2023, 9, 20, 1, 222, 2" "gid")
!(sql.insert &sql_space  ((musicbrainz.artist (gid "uuid_generate_v4()"))
(musicbrainz.artist (name "some name")) (musicbrainz.artist (sort_name "name some")) (musicbrainz.artist (begin_date_year 1983)) (musicbrainz.artist (begin_date_month 9))
(musicbrainz.artist (begin_date_day 20)) (musicbrainz.artist (type 1)) (musicbrainz.artist (area 222)) (musicbrainz.artist (gender 2))))

