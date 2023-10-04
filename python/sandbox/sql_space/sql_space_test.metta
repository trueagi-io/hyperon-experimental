!(extend-py! sql_space)
; database can be installed with use of   https://github.com/metabrainz/musicbrainz-docker#publish-ports-of-all-services
; the description https://musicbrainz.org/doc/MusicBrainz_Database
!(bind! &sql_space (new-sql-space musicbrainz_db localhost musicbrainz musicbrainz 5432))
; save sql query results into given space
!(sql.save-query-result &sql_space &self (, (musicbrainz.artist (id $id) (name $name)) (musicbrainz.artist (id $id) (begin_date_year 1977)) (limit 3)))
!(get-atoms &self)
;result : [GroundingSpace, ((musicbrainz.artist (id "127482") (name "Kanye West"))), ((musicbrainz.artist (id "127482") (begin_date_year 1977))), ((musicbrainz.artist (id "23366") (name "The Dirty Dozen Brass Band"))), ((musicbrainz.artist (id "23366") (begin_date_year 1977))), ((musicbrainz.artist (id "35629") (name "Fabolous"))), ((musicbrainz.artist (id "35629") (begin_date_year 1977)))]

!(match &sql_space (, (musicbrainz.artist (id $id) (name $name)) (musicbrainz.artist (id $id) (begin_date_year 1983)) (limit 3)) $name)
;result ["NOFX", "Red Hot Chili Peppers", "Bon Jovi"]
!(sql.insert &sql_space "musicbrainz.artist"
 "gid, name, sort_name, begin_date_year, begin_date_month, begin_date_day, type, area, gender" "uuid_generate_v4(),'some name', 'name some', 2023, 9, 20, 1, 222, 2" "gid")
!(sql.insert &sql_space  ((musicbrainz.artist (gid "uuid_generate_v4()"))
(musicbrainz.artist (name "some name")) (musicbrainz.artist (sort_name "name some")) (musicbrainz.artist (begin_date_year 1988)) (musicbrainz.artist (begin_date_month 1))
(musicbrainz.artist (begin_date_day 1)) (musicbrainz.artist (type 1)) (musicbrainz.artist (area 222)) (musicbrainz.artist (gender 2))))

