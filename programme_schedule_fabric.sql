-- extracting programme schedule for 2014-07-06 from fabric
WITH CTE AS (
SELECT DISTINCT
PE.billed_title
,publication_date
,publication_start_time
,publication_end_time
,CASE WHEN LEFT((publication_end_time-publication_start_time)::text,1)='-' THEN publication_end_time-publication_start_time + '24:00:00' ELSE publication_end_time-publication_start_time END as duration
,SERV.name as channel
,case 
  when (GENRE.genre ~ 'Current Affairs' or GENRE.genre ~ 'News' or GENRE.genre ~ 'Politic' or GENRE.genre ~ 'Prime Ministerial') then 'News'
  when GENRE.genre ~ 'Children' then 'Children''s'
  when GENRE.genre ~ 'Drama' then 'Drama'
  when (GENRE.genre ~ 'Light Ent' or GENRE.genre ~ 'Outside Broadcast' or GENRE.genre ~ 'Opt Outs') then 'Entertainment'
  when (GENRE.genre ~ 'Features / Factual' or GENRE.genre ~ 'General Programmes') then 'Factual'
  when (GENRE.genre ~ 'Education' or GENRE.genre ~ 'Schools' or GENRE.genre ~ 'Language') then 'Learning'
  when (GENRE.genre ~ 'Music' or GENRE.genre ~ 'Dance') then 'Music'
  when GENRE.genre ~ 'Religio' then 'Religion & Ethics'
  when GENRE.genre ~ 'Sport' then 'Sport'
  when (GENRE.genre ~ 'Comedy' or GENRE.genre ~ 'Sitcom' or GENRE.genre ~ 'Sit Com') then 'Comedy'
  else null
end da_genre_from_enh
FROM em3.publication_event AS PE 
INNER JOIN ref.ref_service AS SERV ON SERV.id=PE.service_id
INNER JOIN em3.product_version AS PV ON PV.id=PE.product_version_id
INNER JOIN em3.media_asset AS MA ON MA.id=PV.media_asset_id
LEFT JOIN dev.fdi_31_enhanced_genre AS GENRE ON GENRE.media_asset_id=MA.id
WHERE publication_date='2014-07-06'
AND planned_event_indicator=0
GROUP BY 
PE.billed_title
,publication_date
,publication_start_time
,publication_end_time
,SERV.name 
,case 
  when (GENRE.genre ~ 'Current Affairs' or GENRE.genre ~ 'News' or GENRE.genre ~ 'Politic' or GENRE.genre ~ 'Prime Ministerial') then 'News'
  when GENRE.genre ~ 'Children' then 'Children''s'
  when GENRE.genre ~ 'Drama' then 'Drama'
  when (GENRE.genre ~ 'Light Ent' or GENRE.genre ~ 'Outside Broadcast' or GENRE.genre ~ 'Opt Outs') then 'Entertainment'
  when (GENRE.genre ~ 'Features / Factual' or GENRE.genre ~ 'General Programmes') then 'Factual'
  when (GENRE.genre ~ 'Education' or GENRE.genre ~ 'Schools' or GENRE.genre ~ 'Language') then 'Learning'
  when (GENRE.genre ~ 'Music' or GENRE.genre ~ 'Dance') then 'Music'
  when GENRE.genre ~ 'Religio' then 'Religion & Ethics'
  when GENRE.genre ~ 'Sport' then 'Sport'
  when (GENRE.genre ~ 'Comedy' or GENRE.genre ~ 'Sitcom' or GENRE.genre ~ 'Sit Com') then 'Comedy'
  else null
end
)
SELECT 
billed_title
,publication_date
,publication_start_time
,publication_end_time
,duration
,channel
,STRING_AGG(da_genre_from_enh, ', ') as genres
FROM CTE 
GROUP BY 
billed_title
,publication_date
,publication_start_time
,publication_end_time
,duration
,channel
ORDER BY publication_start_time