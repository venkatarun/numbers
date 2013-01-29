REGISTER 'lib/httpcore-4.2.1.jar';
REGISTER 'lib/jsoup-1.7.2-SNAPSHOT.jar';
REGISTER 'dist/lib/commoncrawl-examples-1.0.1.jar';

-- pages = LOAD '/data/public/common-crawl/parse-output/segment/*/*arc*' USING org.commoncrawl.pig.ArcLoader() AS (date, length, type, statuscode, ipaddress, url, html);

pages = LOAD '$INPUT' USING org.commoncrawl.pig.ArcLoader() AS (date, length, type, statuscode, ipaddress, url, html);

webpages = FILTER pages BY type == 'text/html';

parsed = FOREACH webpages GENERATE url, nl.vu.few.ParseHTML(html) AS page_info;


non_empty = FILTER parsed BY page_info IS NOT NULL;

input_lines = FOREACH non_empty GENERATE page_info.$1 AS line;

tuples = FOREACH input_lines GENERATE TOKENIZE(LOWER(TRIM(line)), ' ",()*\'\t\n\r\f_#\\/!#') AS tokens;

-- Put each word in its own tuple, put each tuple on a separate row
words = FOREACH tuples GENERATE FLATTEN(tokens) AS word;

ranges = FILTER words BY word MATCHES '^[0-9]+-[0-9]+$';

years = FOREACH ranges GENERATE TOKENIZE(word,'-') AS tokens;
years2 = FOREACH years GENERATE FLATTEN(tokens) AS word;

numbers = UNION words, years2;

filtered_numbers = FILTER numbers BY $0 MATCHES '^([-+]?[0-9]*[.]?[0-9]+(e[-+]?[0-9]+)?)([.]?)$'; -- naive number finding first
-- numbers = FILTER words BY word MATCHES '^[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?$';

floats = FOREACH filtered_numbers GENERATE (float) REGEX_EXTRACT($0, '^([-+]?[0-9]*[.]?[0-9]+(e[-+]?[0-9]+)?)([.]?)$', 1) AS word;--, $0 as orig;

-- create a group for each word
word_groups = GROUP floats BY word;
 
-- count the entries in each group
word_count = FOREACH word_groups GENERATE COUNT(floats) AS count, group AS word;
 
-- order the records by count
ordered_word_count = ORDER word_count BY count DESC;
STORE ordered_word_count INTO '$OUTPUT';
