-- Create staging_311: this table stores the 311 dataset
CREATE TABLE staging_311 (
    case_id VARCHAR(1000),
    case_status VARCHAR(1000),
    city VARCHAR(1000),
    department_name VARCHAR(1000),
    latitude VARCHAR(1000),
    longitude VARCHAR(1000),
    on_time_flag VARCHAR(1000),
    request_closed_date VARCHAR(1000),
    request_open_date VARCHAR(1000),
    service_name VARCHAR(1000),
    service_type VARCHAR(1000),
    sla_target_dt VARCHAR(1000),
    state VARCHAR(1000),
	street_address VARCHAR(1000),
    ward VARCHAR(1000),
    zipcode VARCHAR(1000),
    resolution_days VARCHAR(1000),
    case_status_clean VARCHAR(1000),
    service_type_clean VARCHAR(1000),
    ward_clean VARCHAR(1000),
    department_clean VARCHAR(1000),
    street_address_clean VARCHAR(1000),
    zipcode_clean VARCHAR(1000)
);


SELECT 
    l.city,
    d.year,
    d.month,
    SUM(f.request_count) AS monthly_requests
FROM service_requests_fact AS f
JOIN date_dim AS d 
    ON f.date_dim_id = d.date_dim_id
JOIN location_dim AS l
    ON f.location_dim_id = l.location_dim_id
WHERE UPPER(l.city) IN ('BOSTON', 'WASHINGTON')
GROUP BY 
    l.city,
    d.year,
    d.month
ORDER BY 
    l.city,
    d.year,
    d.month;




-- Preview the first 40 rows from the staging table
SELECT * FROM staging_311 LIMIT 40;


-- Check how many total records were loaded into staging_311
SELECT COUNT(*) FROM staging_311;



-- Create Date_DIM 
CREATE TABLE Date_DIM (
    Date_DIM_ID SERIAL PRIMARY KEY,
    Date_ID INTEGER,
    Full_Date DATE,
    Year INTEGER,
    Month INTEGER,
    Day INTEGER,
    Day_Of_Week INTEGER
);

-- Insert generated calendar dates into the Date_DIM table
INSERT INTO Date_DIM (
    Date_ID, Full_Date, Year, Month, Day, Day_Of_Week
)
SELECT
	-- Generate date ID in YYYYMMDD format
    TO_CHAR(full_date, 'YYYYMMDD')::int AS Date_ID,
	 -- The actual date value
    full_date AS Full_Date,
	-- Extract the year from the date
    EXTRACT(YEAR FROM full_date)::int AS Year,
	-- Extract the month from the date
    EXTRACT(MONTH FROM full_date)::int AS Month,
	-- Extract the day from the date
    EXTRACT(DAY FROM full_date)::int AS Day,
	-- Extract the day from the date
    EXTRACT(DOW FROM full_date)::int AS Day_Of_Week
FROM generate_series(
		-- Start with the earliest request_open_date 
        (SELECT MIN(request_open_date)::date FROM staging_311),
		-- Start with the latest request_open_date 
        (SELECT MAX(request_open_date)::date FROM staging_311),
		-- One dat at a time
        interval '1 day'
    ) AS full_date;

-- Check the date range generated 
SELECT 
    MIN(full_date) AS earliest_date,
    MAX(full_date) AS latest_date
FROM Date_DIM;

-- Preview the first 20 dates 
SELECT full_date 
FROM Date_DIM
ORDER BY full_date
LIMIT 20;


-- Preview the first 40 rows
SELECT * FROM Date_DIM LIMIT 40;

-- Count the total number of dates generated
SELECT COUNT(*) FROM Date_DIM;




/*  
When I was building the Date Dimension, I discussed the design with ChatGPT to confirm whether my approach made sense. 
After loading the data, I noticed that each combination of OpenDate and CloseDate produced a different Date Dimension ID. 
This meant that one OpenDate might only appear once, while the CloseDate could appear many times with different timestamps, 
resulting in many different IDs.

I asked ChatGPT whether this structure was too complicated and whether I should simplify it by switching to a calendar-style Date Dimension, 
where each calendar day appears only once with a single ID. 
ChatGPT explained that the calendar version is common in industry settings because it is cleaner and easier to maintain.

*/


--- Create Location_DIM 
CREATE TABLE Location_DIM (
    Location_DIM_ID SERIAL PRIMARY KEY,
    City VARCHAR(100),
    State VARCHAR(10),
    ZipCode VARCHAR(20),
    Ward VARCHAR(50)
);

-- Insert unique and cleaned location values into the Location Dimension
INSERT INTO Location_DIM (City, State, ZipCode, Ward)
SELECT DISTINCT
	-- Standardize city names
    INITCAP(TRIM(city)) AS City,
	-- Standardize state names
    UPPER(TRIM(state)) AS State,
	-- Clean the zipcode
    NULLIF(TRIM(zipcode_clean), '') AS ZipCode,
	-- Clean the ward
    NULLIF(TRIM(ward_clean), '') AS Ward
FROM staging_311
WHERE TRIM(city) <> '';


/*  
ChatGPT recommed me WHERE TRIM(city) <> '' to ensure that only 
valid and non-empty city values are inserted into the Location Dimension.
*/

-- List all unique city and state
SELECT DISTINCT city, state
FROM Location_DIM
ORDER BY state, city;

-- Count the total number of location records 
SELECT COUNT(*) FROM Location_DIM;

-- Preview the first 20 rows 
SELECT * FROM Location_DIM LIMIT 20;



--- Create Department_DIM
CREATE TABLE Department_DIM (
    Department_DIM_ID SERIAL PRIMARY KEY,
    Department_ID VARCHAR(200),        
    Department_Name VARCHAR(200),      
    Valid_From DATE,
    Valid_To DATE,
    Is_Current BOOLEAN
);
-- Insert cleaned and unique department records 
INSERT INTO Department_DIM (
    Department_ID,
    Department_Name,
    Valid_From,
    Valid_To,
    Is_Current
)
SELECT DISTINCT
	-- Clean the department ID
    NULLIF(TRIM(department_clean), '') AS Department_ID,
	-- Clean the department name
    NULLIF(TRIM(department_name), '') AS Department_Name,
	-- Effective date
    CURRENT_DATE AS Valid_From,
	-- NULL end date
    NULL::date AS Valid_To,
	-- Mark this as the current active version of the department
    TRUE AS Is_Current
FROM staging_311
-- Exclude invalid or blank department values
WHERE department_clean IS NOT NULL
  AND TRIM(department_clean) <> '';

-- Check how many department records
SELECT COUNT(*) FROM Department_DIM;
-- View all rows 
SELECT * FROM Department_DIM;


-- Create Service_DIM
DROP TABLE IF EXISTS Service_DIM;

CREATE TABLE Service_DIM (
    Service_DIM_ID SERIAL PRIMARY KEY,
    Service_ID VARCHAR(20),           
    Service_Name VARCHAR(200),
    Service_Type VARCHAR(200),
    Valid_From DATE,
    Valid_To DATE,
    Is_Current BOOLEAN
);

TRUNCATE Service_DIM;

INSERT INTO Service_DIM (
    Service_ID,
    Service_Name,
    Service_Type,
    Valid_From,
    Valid_To,
    Is_Current
)
SELECT
    'S' || LPAD(ROW_NUMBER() OVER (ORDER BY service_type)::text, 3, '0') AS Service_ID,
    service_name,
    service_type,
    CURRENT_DATE AS Valid_From,
    NULL::date AS Valid_To,
    TRUE AS Is_Current
FROM (
    SELECT DISTINCT
        NULLIF(TRIM(service_name), '') AS service_name,
        NULLIF(TRIM(service_type_clean), '') AS service_type
    FROM staging_311
    WHERE TRIM(service_type_clean) <> ''
) t;


/* 
ChatGPT showed me how to use this by explaining that I could generate a clean, readable ID using:
S' || LPAD(ROW_NUMBER() OVER (ORDER BY service_type_clean)::text, 3, '0') AS Service_ID.
It taught me that this approach creates consistent IDs that are easier to manage.
*/

-- Check how many records
SELECT COUNT(*) FROM Service_DIM;

-- Preview the first 10 rows
SELECT * 
FROM Service_DIM
ORDER BY service_id
LIMIT 10;



--- Create Service_Requests_Fact

CREATE TABLE Service_Requests_Fact (
    Fact_ID SERIAL PRIMARY KEY,

    Date_DIM_ID INTEGER,
    Location_DIM_ID INTEGER,
    Department_DIM_ID INTEGER,
    Service_DIM_ID INTEGER,

    Request_Count INTEGER,
    Resolution_Time_Days DECIMAL(10,2),
    On_Time_Flag BOOLEAN
);

INSERT INTO Service_Requests_Fact (
    Date_DIM_ID,
    Location_DIM_ID,
    Department_DIM_ID,
    Service_DIM_ID,
    Request_Count,
    Resolution_Time_Days,
    On_Time_Flag
)
SELECT
    d.date_dim_id,
    l.location_dim_id,
    dept.department_dim_id,
    s.service_dim_id,
	 -- One record in staging
    1 AS Request_Count,
	-- Convert resolution_days from text to numeric
    CAST(s311.resolution_days AS DECIMAL(10,2)) AS Resolution_Time_Days,
	-- Convert ONTIME / not ONTIME text into a proper boolean flag
    CASE WHEN s311.on_time_flag = 'ONTIME' THEN TRUE ELSE FALSE END AS On_Time_Flag
FROM staging_311 AS s311

-- Date DIM
LEFT JOIN Date_DIM AS d
    ON d.full_date = s311.request_open_date::date

-- Location DIM 
LEFT JOIN Location_DIM AS l
    ON l.city    = INITCAP(TRIM(s311.city))
   AND l.state   = UPPER(TRIM(s311.state))
   AND l.zipcode = NULLIF(TRIM(s311.zipcode_clean), '')
   AND l.ward    = NULLIF(TRIM(s311.ward_clean), '')

-- Department DIM 
LEFT JOIN Department_DIM AS dept
    ON dept.department_id = NULLIF(TRIM(s311.department_clean), '')

-- Service DIM 
LEFT JOIN Service_DIM AS s
    ON s.service_name = NULLIF(TRIM(s311.service_name), '')
   AND s.service_type = NULLIF(TRIM(s311.service_type_clean), '');

-- Check total rows
SELECT COUNT(*) FROM Service_Requests_Fact;

-- Preview first 50 rows
SELECT * FROM Service_Requests_Fact limit 50 ;

-- -Count records is NULL 
SELECT
  SUM(CASE WHEN location_dim_id IS NULL THEN 1 ELSE 0 END) AS no_location,
  SUM(CASE WHEN department_dim_id IS NULL THEN 1 ELSE 0 END) AS no_department,
  SUM(CASE WHEN service_dim_id IS NULL THEN 1 ELSE 0 END) AS no_service
FROM Service_Requests_Fact;



--- Create Service_Performance_Fact
CREATE TABLE Service_Performance_Fact (
    Perf_ID SERIAL PRIMARY KEY,
    Date_DIM_ID INTEGER,
    Location_DIM_ID INTEGER,
    Department_DIM_ID INTEGER,
    Service_DIM_ID INTEGER,
    
    Avg_Resolution_Time DECIMAL(10,2),
    On_Time_Rate DECIMAL(5,2),
    Open_Request_Count INTEGER,
    Case_Closed_Rate DECIMAL(5,2)
);

INSERT INTO Service_Performance_Fact (
    Date_DIM_ID,
    Location_DIM_ID,
    Department_DIM_ID,
    Service_DIM_ID,
    Avg_Resolution_Time,
    On_Time_Rate,
    Open_Request_Count,
    Case_Closed_Rate
)
SELECT
    date_dim_id,
    location_dim_id,
    department_dim_id,
    service_dim_id,
	-- Average time (in days) to resolve service requests
    AVG(resolution_time_days) AS Avg_Resolution_Time,
	-- On-time completion rate (TRUE → 1, FALSE → 0)
    AVG(CASE WHEN on_time_flag = TRUE THEN 1 ELSE 0 END) 
        AS On_Time_Rate,
	-- Total number of service requests
    COUNT(*) AS Open_Request_Count,
	-- Case-closed rate
    AVG(CASE WHEN resolution_time_days IS NOT NULL THEN 1 ELSE 0 END)
        AS Case_Closed_Rate

FROM Service_Requests_Fact
-- Aggregate metrics for each Date × Location × Department × Service combination
GROUP BY
    date_dim_id,
    location_dim_id,
    department_dim_id,
    service_dim_id;

-- Check total rows
SELECT COUNT(*) FROM Service_Performance_Fact;

-- Preview first 50 rows
SELECT * FROM Service_Performance_Fact LIMIT 50;




-- 1. What are the most common service types by city & how do they change by month?


-- Boston 
WITH monthly_counts AS (
    SELECT
        d.month,                          
        s.service_type,                  
        SUM(f.request_count) AS total_requests,
        -- Rank service types within each month by total requests
        ROW_NUMBER() OVER (
            PARTITION BY d.month
            ORDER BY SUM(f.request_count) DESC
        ) AS rn
    FROM Service_Requests_Fact f
    -- Join to Location dimension 
    JOIN Location_DIM l
        ON l.location_dim_id = f.location_dim_id
    -- Join to Date dimension 
    JOIN Date_DIM d
        ON d.date_dim_id = f.date_dim_id
    -- Join to Service dimension 
    JOIN Service_DIM s
        ON s.service_dim_id = f.service_dim_id
	-- Only include Boston requests
    WHERE l.city = 'Boston'               
    GROUP BY d.month, s.service_type)
-- Select only the top service type for each month
SELECT 
    month, 
    service_type, 
    total_requests
FROM monthly_counts
WHERE rn = 1
ORDER BY month;


-- Washington
WITH monthly_counts AS (
    SELECT
        d.month,                          
        s.service_type,                  
        SUM(f.request_count) AS total_requests,
        -- Rank service types within each month by total requests
        ROW_NUMBER() OVER (
            PARTITION BY d.month
            ORDER BY SUM(f.request_count) DESC
        ) AS rn
    FROM Service_Requests_Fact f
    -- Join to Location dimension 
    JOIN Location_DIM l
        ON l.location_dim_id = f.location_dim_id
    -- Join to Date dimension 
    JOIN Date_DIM d
        ON d.date_dim_id = f.date_dim_id
    -- Join to Service dimension 
    JOIN Service_DIM s
        ON s.service_dim_id = f.service_dim_id
	-- Only include Washington requests
    WHERE l.city = 'Washington'               
    GROUP BY d.month, s.service_type)
-- Select only the top service type for each month
SELECT 
    month, 
    service_type, 
    total_requests
FROM monthly_counts
WHERE rn = 1
ORDER BY month;


-- 2. Which service types have the longest average resolution times, and how do these slowest services differ between Boston and Washington, D.C.?

SELECT 
    l.city,
    s.service_type,
	-- Average resolution time
    ROUND(AVG(f.avg_resolution_time), 2) AS avg_resolution_days
FROM Service_Performance_Fact AS f
-- Join to Location dimension 
JOIN Location_DIM AS l
    ON f.location_dim_id = l.location_dim_id
-- Join to Service dimension 
JOIN Service_DIM As s
    ON f.service_dim_id = s.service_dim_id
-- Group results by each city and service type
GROUP BY l.city, s.service_type
-- Only include groups where we have valid average resolution data
HAVING AVG(f.avg_resolution_time) IS NOT NULL
-- Sort results by city, then by longest average resolution time
ORDER BY l.city, avg_resolution_days DESC;


SELECT s.service_type, COUNT(*) AS count
FROM Service_DIM AS s
JOIN Service_Requests_Fact AS f
    ON f.service_dim_id = s.service_dim_id
JOIN Location_DIM AS l
	ON l.location_dim_id = f.location_dim_id
WHERE l.city = 'Boston'
GROUP BY s.service_type
ORDER BY s.service_type;



-- 3. How do service request volumes vary across ZIP codes or wards within each city?



WITH zip_stats AS (
    SELECT
        l.zipcode,
		-- Total requests coming from Boston for this ZIP code
        SUM(CASE WHEN l.city = 'Boston' THEN f.request_count END) AS boston_requests,
		-- Total requests coming from Washington for this ZIP code
        SUM(CASE WHEN l.city = 'Washington' THEN f.request_count END) AS dc_requests
    FROM Service_Requests_Fact AS 
	-- Join to Location dimension 
    JOIN Location_DIM AS l 
		ON l.location_dim_id = f.location_dim_id
	-- Exclude invalid or missing ZIP codes
    WHERE l.zipcode <> 'Unknown'
	-- Aggregate metrics at the ZIP code level
    GROUP BY l.zipcode
)
SELECT *
FROM zip_stats
-- Order results so non-NULL values appear first
ORDER BY
    CASE WHEN boston_requests IS NOT NULL THEN 0 ELSE 1 END,
    boston_requests DESC NULLS LAST,
    CASE WHEN dc_requests IS NOT NULL THEN 0 ELSE 1 END,
    dc_requests DESC NULLS LAST;



-- 4. Which ZIP codes have the lowest on-time performance in each city, and does high request volume correlate with lower on-time rates?

SELECT 
    l.city,
    l.zipcode,
	-- Total number of service requests for this ZIP code
    SUM(f.open_request_count) AS total_requests,
    -- Average on-time rate for the ZIP code
	ROUND( AVG(CASE WHEN f.on_time_rate IS NOT NULL THEN f.on_time_rate END), 3
    ) AS avg_on_time_rate
FROM Service_Performance_Fact AS f
-- Join to Location Dimension 
JOIN Location_DIM AS l 
	on l.location_dim_id = f.location_dim_id
-- Aggregate performance at the city × ZIP level
GROUP BY l.city, l.zipcode
ORDER BY l.city, avg_on_time_rate ASC, total_requests DESC;





-- 5. Which types of services show biggest response-time differences between Boston & DC?

SELECT
    l.city,
    -- Average number of days it takes to resolve service requests
    ROUND(AVG(f.avg_resolution_time), 2) AS avg_resolution_days,
    -- Average on-time completion rate for the city
    ROUND(AVG(f.on_time_rate), 3) AS avg_on_time_rate

FROM Service_Performance_Fact AS f
-- Join to Location_DIM
JOIN Location_DIM AS l
	on l.location_dim_id = f.location_dim_id
-- Aggregate metrics at the city level
GROUP BY l.city
ORDER BY l.city;



