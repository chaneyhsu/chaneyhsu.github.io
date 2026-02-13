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




-- Preview the first 40 rows from the staging table
SELECT * FROM staging_311 LIMIT 40;

-- Check how many total records were loaded into staging_311
SELECT COUNT(*) FROM staging_311;


-- Create Star Schema 

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
DROP TABLE IF EXISTS Location_DIM;
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
DROP TABLE IF EXISTS Department_DIM;

CREATE TABLE Department_DIM (
    Department_DIM_ID SERIAL PRIMARY KEY,
    Department_ID VARCHAR(50),
    Valid_From DATE,
    Valid_To DATE,
    Is_Current BOOLEAN,
    Department_Name_Current VARCHAR(100),
    Department_Name_Previous VARCHAR(100),
    Change_Reason VARCHAR(100)
);

INSERT INTO Department_DIM (
    Department_ID,
    Valid_From,
    Valid_To,
    Is_Current,
    Department_Name_Current,
    Department_Name_Previous,
    Change_Reason
)
SELECT DISTINCT
	-- Clean the department ID
    NULLIF(TRIM(department_clean), '') AS Department_ID,
	-- Effective date
    CURRENT_DATE AS Valid_From,
	-- NULL end date
    NULL::date AS Valid_To,
	-- Mark this as the current active version of the department
    TRUE AS Is_Current,
    NULLIF(TRIM(department_name), '') AS Department_Name_Current,
    NULL AS Department_Name_Previous,      
    NULL AS Change_Reason                  
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
DROP TABLE IF EXISTS Service_Requests_Fact;
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
LEFT JOIN Date_DIM AS d ON d.full_date = s311.request_open_date::date
-- Location DIM 
LEFT JOIN Location_DIM AS l ON l.city    = INITCAP(TRIM(s311.city))
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
SELECT * FROM Service_Requests_Fact limit 50;

-- -Count records is NULL 
SELECT
  SUM(CASE WHEN location_dim_id IS NULL THEN 1 ELSE 0 END) AS no_location,
  SUM(CASE WHEN department_dim_id IS NULL THEN 1 ELSE 0 END) AS no_department,
  SUM(CASE WHEN service_dim_id IS NULL THEN 1 ELSE 0 END) AS no_service
FROM Service_Requests_Fact;









-- Create normalization Schema 
 
-- Create Department Table
CREATE TABLE Department (
    DepartmentID SERIAL PRIMARY KEY,
    DepartmentName VARCHAR(200)
);

INSERT INTO Department(DepartmentName)
SELECT DISTINCT TRIM(department_name)
FROM staging_311
WHERE TRIM(department_name)<>'';

SELECT * FROM Department;
SELECT COUNT(*) FROM Department



-- Create CaseStatus Table
DROP TABLE IF EXISTS CaseStatus;
CREATE TABLE CaseStatus(
    CaseStatus_ID SERIAL PRIMARY KEY,
    Status VARCHAR(200)
);


INSERT INTO CaseStatus(Status)
SELECT DISTINCT TRIM(case_status_clean)
FROM staging_311
WHERE TRIM(case_status_clean)<>'';

SELECT * FROM CaseStatus;
SELECT COUNT(*) FROM CaseStatus


-- Create OnTimeFlag Table
CREATE TABLE OnTimeFlag(
    OnTimeFlagID SERIAL PRIMARY KEY,
    Flag VARCHAR(200)
);

INSERT INTO OnTimeFlag(Flag)
SELECT DISTINCT TRIM(on_time_flag)
FROM staging_311
WHERE TRIM(on_time_flag)<>'';

SELECT * FROM  OnTimeFlag;
SELECT COUNT(*) FROM OnTimeFlag

-- Create Service Table
DROP TABLE IF EXISTS Service;
CREATE TABLE Service(
    ServiceID SERIAL PRIMARY KEY,
    ServiceType VARCHAR(200)
);

INSERT INTO Service(ServiceType)
SELECT DISTINCT TRIM(service_type_clean)
FROM staging_311
WHERE TRIM(service_type_clean)<>'';


SELECT * FROM Service;
SELECT COUNT(*) FROM Service


-- Create Location Table 
DROP TABLE IF EXISTS Location ;
CREATE TABLE Location (
    LocationID SERIAL PRIMARY KEY,
    CityID VARCHAR(200),
    Zipcode VARCHAR(50)
);

INSERT INTO Location (CityID, Zipcode)
SELECT DISTINCT
    TRIM(city),
    TRIM(zipcode_clean)
FROM staging_311;


SELECT * FROM Location;
SELECT COUNT(*) FROM Location;

-- Create ServiceRequest Table 
DROP TABLE IF EXISTS ServiceRequest;

CREATE TABLE ServiceRequest (
    RequestID SERIAL PRIMARY KEY,
    Case_ID VARCHAR(200),  
    CaseStatusID INTEGER REFERENCES CaseStatus(CaseStatus_ID),
    Resolution_Days DECIMAL(10,2),
    OnTimeFlagID INTEGER REFERENCES OnTimeFlag(OnTimeFlagID),
    ServiceID INTEGER REFERENCES Service(ServiceID),
    DepartmentID INTEGER REFERENCES Department(DepartmentID),
    LocationID INTEGER REFERENCES Location(LocationID)
);


INSERT INTO ServiceRequest (
    Case_ID,
    CaseStatusID,
    Resolution_Days,
    OnTimeFlagID,
    ServiceID,
    DepartmentID,
    LocationID
)
SELECT
    raw.case_id,
    cs.CaseStatus_ID,
    CAST(raw.resolution_days AS DECIMAL(10,2)),
    ot.OnTimeFlagID,
    sv.ServiceID,
    dp.DepartmentID,
    lc.LocationID
FROM staging_311 raw
LEFT JOIN CaseStatus cs ON TRIM(raw.case_status_clean) = cs.Status
LEFT JOIN OnTimeFlag ot ON TRIM(raw.on_time_flag) = ot.Flag
LEFT JOIN Service sv ON TRIM(raw.service_type_clean) = sv.ServiceType
LEFT JOIN Department dp ON TRIM(raw.department_name) = dp.DepartmentName
LEFT JOIN Location lc ON TRIM(raw.city) = lc.CityID
      AND TRIM(raw.zipcode_clean) = lc.Zipcode;

SELECT * FROM ServiceRequest;
SELECT COUNT(*) FROM ServiceRequest;

-- Create ServiceRequest_Dates table
DROP TABLE IF EXISTS ServiceRequest_Dates;

CREATE TABLE ServiceRequest_Dates (
    Request_ID INTEGER PRIMARY KEY REFERENCES ServiceRequest(RequestID),
    OpenDate DATE,
    CloseDate DATE,
    TargetDate DATE
);

INSERT INTO ServiceRequest_Dates (
    Request_ID,
    OpenDate,
    CloseDate,
    TargetDate
)
SELECT
    sr.RequestID,
    raw.request_open_date::date,
    raw.request_closed_date::date,
    raw.sla_target_dt::date
FROM staging_311 raw
JOIN ServiceRequest sr ON sr.Case_ID = raw.case_id;

SELECT * FROM ServiceRequest_Dates;
SELECT COUNT(*) FROM ServiceRequest_Dates;



-- 1. Which service types occur most frequently each month 
-- Star Schema
-- Find the most requested service type for each month in Washington
WITH MonthlyCounts AS (
    SELECT 
        d.year,
        d.month,
        s.service_type,
        COUNT(*) AS request_count,
        ROW_NUMBER() OVER (
            PARTITION BY d.year, d.month
            ORDER BY COUNT(*) DESC
        ) AS rn
    FROM Service_Requests_Fact AS SRF
    JOIN Date_DIM AS d ON SRF.date_dim_id = d.date_dim_id
    JOIN Service_DIM AS s ON SRF.service_dim_id = s.service_dim_id
    JOIN Location_DIM AS l ON SRF.location_dim_id = l.location_dim_id
    WHERE l.city = 'Washington'
    GROUP BY 
        d.year, d.month, s.service_type
)
SELECT 
    year,
    month,
    service_type,
    request_count
FROM MonthlyCounts
WHERE rn = 1
ORDER BY year, month;


-- Normalization Schema
WITH MonthlyCounts AS (
    SELECT
        EXTRACT(YEAR FROM d.OpenDate) AS year,
        EXTRACT(MONTH FROM d.OpenDate) AS month,
        s.ServiceType,
        COUNT(*) AS request_count,
        ROW_NUMBER() OVER (
            PARTITION BY EXTRACT(YEAR FROM d.OpenDate),
                         EXTRACT(MONTH FROM d.OpenDate)
            ORDER BY COUNT(*) DESC
        ) AS rn
    FROM ServiceRequest sr
    JOIN ServiceRequest_Dates d ON sr.RequestID = d.Request_ID
    JOIN Service s ON sr.ServiceID = s.ServiceID
    JOIN Location lc ON sr.LocationID = lc.LocationID
    WHERE lc.CityID = 'WASHINGTON'
    GROUP BY 
        year, month, s.ServiceType
)

SELECT 
    year,
    month,
    ServiceType,
    request_count
FROM MonthlyCounts
WHERE rn = 1
ORDER BY year, month;




-- 2. Which zipcodes generate the highest volume of requests?

-- Star Schema
SELECT 
	l.zipcode AS zipcode,
	COUNT(*) AS request_count
FROM Service_Requests_Fact AS SRF
JOIN Location_DIM AS l on l.location_dim_id = SRF.location_dim_id
WHERE l.city= 'Boston'
GROUP BY l.zipcode
ORDER BY request_count DESC;



-- Normalization Schema
SELECT
    l.Zipcode,
    COUNT(*) AS request_count
FROM ServiceRequest AS sr
JOIN Location AS l ON sr.LocationID = l.LocationID
WHERE l.CityID = 'Boston'      
GROUP BY l.Zipcode
ORDER BY request_count DESC;



-- 3. Which departments with the shortest average resolution time
-- Star Schema
SELECT
    dept.department_name_current AS department_name,
    ROUND(AVG(f.resolution_time_days), 2) AS avg_resolution_days
FROM Service_Requests_Fact AS f
JOIN Department_DIM AS dept ON f.department_dim_id = dept.department_dim_id
GROUP BY dept.department_name_current
ORDER BY avg_resolution_days ASC;

-- Normalized Schema
SELECT
    d.departmentname AS department_name,
    ROUND(AVG(sr.resolution_days), 2) AS avg_resolution_days
FROM ServiceRequest AS sr
JOIN Department AS d ON sr.departmentid = d.departmentid
GROUP BY d.departmentname
ORDER BY avg_resolution_days ASC;


-- Compare the average resolution time and on-time performance for PWD
-- Star Schema Version
SELECT
    s.service_type,
    ROUND(AVG(f.resolution_time_days), 2) AS avg_resolution_days,
    ROUND(AVG(CASE WHEN f.on_time_flag THEN 1 ELSE 0 END), 3) AS on_time_rate,
    COUNT(*) AS total_requests
FROM Service_Requests_Fact f
JOIN Service_DIM s ON f.service_dim_id = s.service_dim_id
JOIN Date_DIM d ON f.date_dim_id = d.date_dim_id
JOIN Department_DIM dept ON f.department_dim_id = dept.department_dim_id
JOIN Location_DIM loc ON f.location_dim_id = loc.location_dim_id
WHERE loc.city = 'Boston'
  AND dept.department_name_current = 'PWD'
  AND d.year = 2024
  AND d.month = 4
GROUP BY s.service_type
ORDER BY avg_resolution_days ASC;


-- Normalized Schema Version
SELECT
    s.ServiceType,
    ROUND(AVG(sr.Resolution_Days), 2) AS avg_resolution_days,
    ROUND(AVG(CASE WHEN ot.Flag = 'ONTIME' THEN 1 ELSE 0 END), 3) AS on_time_rate,
    COUNT(*) AS total_requests
FROM ServiceRequest sr
JOIN Service s ON sr.ServiceID = s.ServiceID
JOIN Department d ON sr.DepartmentID = d.DepartmentID
JOIN Location l ON sr.LocationID = l.LocationID
JOIN OnTimeFlag ot ON sr.OnTimeFlagID = ot.OnTimeFlagID
JOIN ServiceRequest_Dates dt ON sr.RequestID = dt.Request_ID
WHERE l.CityID = 'Boston'
  AND d.DepartmentName = 'PWD'
  AND EXTRACT(YEAR FROM dt.OpenDate) = 2024
  AND EXTRACT(MONTH FROM dt.OpenDate) = 4
GROUP BY s.ServiceType
ORDER BY avg_resolution_days ASC;





