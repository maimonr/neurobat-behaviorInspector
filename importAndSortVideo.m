function [rawDataSorted, frameDateTimesSorted] = importAndSortVideo(video_fname)

delimiter = ',';
importFormatSpec = '%f%s%[^\n\r]';
metadataDateFormatSpec = 'MM/dd/yyyy HH:mm:ss:SSSSSS';
metaDataFName = [video_fname(1:end-3) 'ts.csv'];

rawData = importdata(video_fname);
rawData = squeeze(rawData(:,:,1,:));

fileID = fopen(metaDataFName,'r');
metadataArray = textscan(fileID, importFormatSpec, 'Delimiter', delimiter,  'ReturnOnError', false);
fclose(fileID);
frameTimes = metadataArray{:, 2};
frameDateTimes = datetime(frameTimes,'InputFormat',metadataDateFormatSpec);
if length(frameDateTimes) > size(rawData,3)
    frameDateTimes = frameDateTimes(1:size(rawData,3));
end
[frameDateTimesSorted, frameIdx] = sort(frameDateTimes);

rawDataSorted = rawData(:,:,frameIdx);

end