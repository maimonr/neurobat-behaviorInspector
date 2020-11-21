function inspect_audio_video_continuous(exp_dir,varargin)

pnames = {'exp_type','bat_str','event_pos_data','onlyBouts','sessionType','bout_separation_length'};
dflts  = {'adult','bat',[],true,'communication',3};
[exp_type,bat_str,event_pos_data,only_bout_flag,sessionType,bout_separation_length] = internal.stats.parseArgs(pnames,dflts,varargin{:});

h = figure;
call_k = 1;
overlayObjs = [];
useColor = false;
exp_dir = strip(exp_dir,filesep);
exp_dir_split = strsplit(exp_dir,filesep);
expDate = datetime(exp_dir_split{end},'InputFormat','MMddyyyy');
exp_date_str = datestr(expDate,'dd-mmm-yyyy');
baseDir = fullfile(exp_dir_split{1:end-1});
call_data_dir = fullfile(baseDir,'call_data');
bhvDir = fullfile(baseDir,'bhv_data');
switch exp_type
    case {'adult','adult_operant'}
        nVideo = 2;
        useOverlay = false;
        overlay_dir_strs = {};
        camStrs = {'Camera 1','Camera 2'};
        switch exp_type
            case 'adult'
                cam_dir_strs = {'video',sessionType};
            case 'adult_operant'
                cam_dir_strs = {'video'};
        end
        event_pos_fname_str = 'cut_call_data';
    case 'adult_social'
        switch sessionType
            case 'social' % vocal session not implemented yet
                nVideo = 1;
                useOverlay = true;
                camStrs = {'infrared'};
                cam_dir_strs = {'video',sessionType,'infrared'};
                overlay_dir_strs = {'video',sessionType,'color'};
                overlay_cam_strs = {'color'};
                overlayObjs = load('IRtoColor_Regist_fast','Rfixed','t');
                useColor = true;
                event_pos_fname_str = 'cut_call_data_social';
        end
end

hAudioTrace = axes('Units','Normalized','Position',[0.13 0.175 0.775 0.15]);
switch nVideo
    case 1
        hMovie = axes('Units','Normalized','Position',[0.15 0.33 0.76 0.56]);
    case 2
        hMovie = [axes('Units','Normalized','Position',[0.15 0.33 0.335 0.56]),...
            axes('Units','Normalized','Position',[0.575 0.33 0.335 0.56])];
end
hCalls = axes('Units','Normalized','Position',[0.2 0.075 0.65 0.075]);

params = struct('audio_fs',250e3,'video_fs',[],'nFrames',[],'nVideo',nVideo,...
    'hAudioTrace',hAudioTrace,'hMovie',hMovie,'hFig',h,'hCalls',hCalls,...
    'bat_str',bat_str,'exp_date',exp_date_str,'exp_type',exp_type,'exp_dir',exp_dir,...
    'eventpos',[],'timestamps_string',[],'useOverlay',useOverlay,...
    'overlayObjs',overlayObjs,'useColor',useColor,'call_info_fname',[]);

setappdata(params.hFig,'currentFrame',1);
setappdata(params.hFig,'startAudio',0);
setappdata(params.hFig,'endAudio',0);
setappdata(params.hFig,'isPlayingVideo',0);
setappdata(params.hFig,'initialize',1);
setappdata(params.hFig,'playbackSpeed',2);
setappdata(params.hFig,'callOffset',1);
setappdata(params.hFig,'timeOffset',0);
setappdata(params.hFig,'imageContrast',0);
setappdata(params.hFig,'call_k',call_k)

[frame_ts_info] = deal(cell(1,nVideo));
for v_k = 1:nVideo
    videoDir = fullfile(exp_dir,cam_dir_strs{:});
    frame_ts_fname = dir(fullfile(videoDir,[strrep(lower(camStrs{v_k}),' ','') '*frame_timestamps_info.mat']));
    assert(length(frame_ts_fname) == 1);
    s = load(fullfile(frame_ts_fname.folder,frame_ts_fname.name));
    frame_ts_info{v_k} = s.frame_ts_info;
    if useOverlay
        v_overlay = 2*v_k;
        videoDir = fullfile(exp_dir,overlay_dir_strs{:});
        frame_ts_fname = dir(fullfile(videoDir,[strrep(lower(overlay_cam_strs{v_k}),' ','') '*frame_timestamps_info.mat']));
        assert(length(frame_ts_fname) == 1);
        s = load(fullfile(frame_ts_fname.folder,frame_ts_fname.name));
        frame_ts_info{v_overlay} = s.frame_ts_info;
    end
end

audio_dir = fullfile(exp_dir, 'audio', sessionType, 'ch1');

if isempty(event_pos_data)
    
    s = load(fullfile(call_data_dir, [datestr(expDate,'yyyymmdd') '_' event_pos_fname_str '.mat']));
    event_pos_data = s.cut_call_data;
    
    if ~any(isnan([event_pos_data.noise]))
        event_pos_data = event_pos_data(~[event_pos_data.noise]);
    end
    
    [event_pos_data.corrected_eventpos] = event_pos_data.corrected_callpos;
    [event_pos_data.file_event_pos] = event_pos_data.callpos;
    
else
    
    if ~any(isnan([event_pos_data.noise]))
        event_pos_data = event_pos_data(~[event_pos_data.noise]);
    end
    
    if ~isfield(event_pos_data,'corrected_eventpos')
        [event_pos_data.corrected_eventpos] = event_pos_data.corrected_callpos;
    end
    if ~isfield(event_pos_data,'file_event_pos')
        [event_pos_data.file_event_pos] = event_pos_data.callpos;
    end
    %     nlg_times = varargin{1};
    %     event_pos_data = get_event_pos_data(exp_dir,nlg_times); % still needs writing!!
    
end

if only_bout_flag
    callPos = vertcat(event_pos_data.corrected_eventpos);
    ICI = [Inf; callPos(2:end,1) - callPos(1:end-1,2)];
    callIdx = ICI > 1e3*bout_separation_length;
    event_pos_data = event_pos_data(callIdx);
end

switch exp_type
    
    case 'piezo'
        call_info_fname = fullfile(audio_dir, ['call_info_' params.bat_str '_' params.exp_date '.mat']);
        nBehaviors = 1;
        call_info = struct('eventpos',num2cell(vertcat(event_pos_data.file_event_pos),2),...
            'corrected_eventpos',num2cell(vertcat(event_pos_data.corrected_eventpos),2),...
            'behaviors',repmat({cell(1,nBehaviors)},length(event_pos_data),1));
        allBehaviorList = {'male','female','unclear'};
        
        params.eventpos = 1e-3*vertcat(event_pos_data.corrected_eventpos)'/60; % call position in minutes
        params.timestamps_string = 'piezo';
        
    case 'nlg'
        allBehaviorList = {'Still','Bite','Shiver','Survey','L2F','Climb','Claw','Voc','Flap',...
            'E','Sniff','Spread','Strike','M2B','nE'};
        nBehaviors = 2;
        call_info_fname = fullfile(audio_dir, ['juv_call_info_' params.bat_str '_' params.exp_date '.mat']);
        call_info = struct('eventpos',num2cell(vertcat(event_pos_data.file_event_pos),2),...
            'corrected_eventpos',num2cell(vertcat(event_pos_data.corrected_eventpos),2),...
            'juvCall',[],'echoCall',[],'behaviors',repmat({cell(1,nBehaviors)},length(event_pos_data),1));
        
        params.eventpos = 1e-3*vertcat(event_pos_data.corrected_eventpos)'/60; % call position in minutes
        params.timestamps_string = 'nlg';
        
    case 'nlg_activity'
        allBehaviorList = {'Resting','Tongue (Grooming)','Tongue (other)','Vocalization','Echo','Locomotion','Active hanging','Wing flap','nE'};
        nBehaviors = 3;
        call_info_fname = fullfile(audio_dir, ['call_info_' params.bat_str '_' params.exp_date '.mat']);
        call_info = struct('eventpos',num2cell(vertcat(event_pos_data.file_event_pos),2),...
            'corrected_eventpos',num2cell(vertcat(event_pos_data.corrected_eventpos),2),...
            'behaviors',repmat({cell(1,nBehaviors)},length(event_pos_data),1));
        
        params.eventpos = 1e-3*vertcat(event_pos_data.corrected_eventpos)'/60; % call position in minutes
        params.timestamps_string = 'nlg';
        
    case {'adult','adult_social'}
        nBehaviors = 1;
        
        allBehaviorList = {'Aggression','Incidental','Probing','Spontaneous','Unclear','Other'};
        call_info_fname = fullfile(bhvDir, ['call_info_' params.bat_str '_' params.exp_date '.mat']);
        call_info = struct('eventpos',num2cell(vertcat(event_pos_data.file_event_pos),2),...
            'corrected_eventpos',num2cell(vertcat(event_pos_data.corrected_eventpos),2),...
            'batsInvolved',[],'behaviors',repmat({cell(1,nBehaviors)},length(event_pos_data),1),...
            'behaviorTime',repmat({cell(1,nBehaviors)},length(event_pos_data),1));
        params.timestamps_string = 'nlg';
        
        params.eventpos = 1e-3*(vertcat(event_pos_data.corrected_eventpos)- frame_ts_info{v_k}.timestamps_nlg(1))'/60; % call position in minutes
end
params.call_info_fname = call_info_fname;

if isfield(event_pos_data,'uniqueID')
    [call_info.callID] = event_pos_data.uniqueID;
else
    callID = num2cell(1:length(call_info));
    [call_info.callID] = callID{:};
end

% try to get existing call_info file

try
    s = load(call_info_fname);
    call_field_names = fieldnames(s.call_info);
    for f = 1:length(call_field_names)
        if isfield(call_info,call_field_names{f})
            [call_info.(call_field_names{f})] = deal(s.call_info.(call_field_names{f}));
        end
    end
    disp(['loading existing file ' call_info_fname]);
catch
    disp(['couldn''t find ' call_info_fname]);
end

% ask for bat ID strings for identity tagging

bat_ID_strs = inputdlg('Enter comma separated bat numbers: ');
bat_ID_strs = bat_ID_strs{1};

if ~isempty(bat_ID_strs)
    bat_ID_strs = strrep(bat_ID_strs,' ','');
    bat_ID_strs = strsplit(bat_ID_strs,',');

    if ~all(cellfun(@length,bat_ID_strs) == 5)
        disp('Bat ID numbers are 5 digit strings, please verify')
        bat_ID_strs = inputdlg('Enter comma separated bat numbers: ');
    end
end

params.bat_IDs = bat_ID_strs;

Y = repmat([0; 1], 1, length(params.eventpos));
plot(hCalls,params.eventpos,Y,'r')
xlim(hCalls,[0 max(params.eventpos(:))])
ylim(hCalls,[0 1.1]);
hCalls.YAxis.Visible = 'off';
hCalls.Box = 'off';
hCalls.Color = get(h,'Color');
set(hCalls,'TickLength',[0 0])
xlabel(hCalls,'Time (min)')
hold(hCalls,'on');
plot(params.hCalls,mean(params.eventpos(:,call_k)),1.1,'vk','MarkerFaceColor','k','Tag','CallMarker');

guidata(params.hFig,call_info);

[audioData,videoData,params] = loadNextCall(event_pos_data,frame_ts_info,params);

initMovie(videoData,audioData,params,audio_dir,event_pos_data,frame_ts_info,allBehaviorList);


colormap('gray')

end

function initMovie(videoData,audioData,params,audio_dir,event_pos_data,frame_ts_info,allBehaviorList)

call_k = getappdata(params.hFig,'call_k');
playbackSpeed = getappdata(params.hFig,'playbackSpeed');
callOffset = getappdata(params.hFig,'callOffset');
setappdata(params.hFig,'currentFrame',1);

a = audioplayer(audioData,params.audio_fs/playbackSpeed);
frame_k = 1;
a.UserData = frame_k;
a.StopFcn = {@stop_video_playback,params};
a.TimerPeriod = playbackSpeed/params.video_fs;
setappdata(params.hFig,'a',a);

figure(params.hFig);
if getappdata(params.hFig,'initialize')
    setappdata(params.hFig,'initialize',0);
else
    UIPanels = findobj(params.hFig,'type','UIPanel');
    UIButtons = findobj(params.hFig,'type','UIControl');
    delete([UIPanels; UIButtons]);
end

insertButtons(params,audio_dir,event_pos_data,frame_ts_info,allBehaviorList);

audio_offset = round(callOffset*params.audio_fs);
call_end_sample = min(audio_offset+diff(event_pos_data(call_k).file_event_pos),length(audioData));
callData = audioData(audio_offset:call_end_sample);

cla(params.hAudioTrace);
plot(params.hAudioTrace,(1:length(audioData)),audioData,'k')
plot(params.hAudioTrace,audio_offset + (1:length(callData)),callData,'r')
xlim(params.hAudioTrace,[0 length(audioData)])
ylim(params.hAudioTrace,[min(audioData) max(audioData)]);
hold(params.hAudioTrace,'on');
axis(params.hAudioTrace,'off');
imObjs = cell(1,params.nVideo);
for s = 1:params.nVideo
    cla(params.hMovie(s))
    if ~isempty(videoData{s})
        vidIdxs = repmat({':'},1,ndims(videoData{s})-1);
        imObjs{s} = imshow(videoData{s}(vidIdxs{:},1),'Parent',params.hMovie(s));
    end
    axis(params.hMovie(s),'tight')
    axis(params.hMovie(s),'square')
end

a.TimerFcn = {@plot_frame_audioplayer_callback, videoData, params.hAudioTrace, imObjs};
delete(findobj(params.hCalls,'Tag','CallMarker'));
plot(params.hCalls,mean(params.eventpos(:,call_k)),1.1,'vk','MarkerFaceColor','k','Tag','CallMarker');

end

function [audioData,videoData,params,success] = loadNextCall(event_pos_data,frame_ts_info,params)
hWait = waitbar(0,'Loading video data');
call_k = getappdata(params.hFig,'call_k');
callOffset = getappdata(params.hFig,'callOffset');
imageContrast = getappdata(params.hFig,'imageContrast');
try
    
    nVideo = params.nVideo;
    call_time = ((event_pos_data(call_k).corrected_eventpos(1)));
    video_fs = zeros(1,nVideo);
    videoData = cell(1,nVideo);
    for v = 1:nVideo
       [videoData{v},video_fs(v)] = loadVideoData(params,frame_ts_info{v},call_time,imageContrast,false);
    end
    
    params.video_fs = mean(video_fs);
    params.nFrames = min(cellfun(@(x) size(x,ndims(x)),videoData));
    
    if params.useOverlay
        waitbar(0,hWait,'Loading overlay data');
        downsampleFactor = 1;
        overlaid_video_data = cell(1,nVideo);
        for v = 1:nVideo
            if ~isempty(videoData{v})
                video_data_overlay = loadVideoData(params,frame_ts_info{v+nVideo},call_time,0,true);
                imSize(1,:) = size(video_data_overlay,[1 2]);
                imSize(2,:) = size(videoData{v},[1 2]);
                imSize = max(imSize);
                overlaid_video_data{v} = zeros(imSize(1)/downsampleFactor,imSize(2)/downsampleFactor,3,params.nFrames,'uint8');
                for frame_k = 1:params.nFrames
                    coRegFrame = imwarp(videoData{v}(:,:,frame_k),params.overlayObjs.t,'OutputView',params.overlayObjs.Rfixed);    % this does the job of co-regist the IR to fit to color.
                    overlayFrame = 3*video_data_overlay(:,:,:,frame_k); % this is done to enhcace the colors. may need adjusting
                    if max(imSize) > 512
                        coRegFrame = coRegFrame(1:downsampleFactor:imSize(1),1:downsampleFactor:imSize(2),:);
                        overlayFrame = overlayFrame(1:downsampleFactor:imSize(1),1:downsampleFactor:imSize(2),:,:);
                    end
                    overlaid_video_data{v}(:,:,:,frame_k) = imadjust((repmat(coRegFrame,1,1,3) + overlayFrame)/2,[0 0.5]);
                end
            end
        end
        videoData = overlaid_video_data;
    end
    
    if iscell(event_pos_data(call_k).fName)
        event_pos_data(call_k).fName = event_pos_data(call_k).fName{1};
        event_pos_data(call_k).f_num = event_pos_data(call_k).f_num(1);
    end
    waitbar(0,hWait,'Loading audio data');
    audio_dir_event_pos = fullfile(event_pos_data(call_k).fName);
    exp_dir_split = strsplit(params.exp_dir,filesep);
    audio_fname_split = strsplit(audio_dir_event_pos,filesep);
    dateIdx = find(ismember(audio_fname_split,exp_dir_split),1,'last');
    audio_fname = fullfile(params.exp_dir,audio_fname_split{dateIdx+1:end});
    audioDir = fileparts(audio_fname);
    
    audio_files = dir(fullfile(audioDir,'T*.WAV'));
    wav_file_nums = cellfun(@(x) str2double(x(end-7:end-4)),{audio_files.name});
    
    audio_offset = round(callOffset*params.audio_fs);
    requested_audio_samples = -audio_offset + event_pos_data(call_k).file_event_pos(1):event_pos_data(call_k).file_event_pos(1) + audio_offset;

    base_audio_data = audioread(audio_fname);
    
    f_num = event_pos_data(call_k).f_num;
    while any(requested_audio_samples <= 0)
        if f_num > 1
            f_num = f_num - 1;
        else
            requested_audio_samples(requested_audio_samples<=0) = 1;
        end
        audio_file_idx = wav_file_nums == f_num;
        pad_audio_data = audioread([audio_files(audio_file_idx).folder filesep audio_files(audio_file_idx).name]);
        nSamples = length(pad_audio_data);
        base_audio_data = [pad_audio_data; base_audio_data];
        requested_audio_samples = requested_audio_samples + nSamples;
        
    end
    
    
    f_num = event_pos_data(call_k).f_num;
    while any(requested_audio_samples > length(base_audio_data))
        
        f_num = f_num + 1;
        audio_file_idx = wav_file_nums == f_num;
        pad_audio_data = audioread([audio_files(audio_file_idx).folder filesep audio_files(audio_file_idx).name]);
        base_audio_data = [base_audio_data; pad_audio_data];
        
    end
    
    audioData = base_audio_data(requested_audio_samples);
catch err
    disp(err)
    keyboard
    success = 0;
    
    audioData = [];
    videoData = [];
    params = [];
    close(hWait);
    return
end

success = 1;
close(hWait);
end

function [videoData,video_fs] = loadVideoData(params,frame_ts_info,call_time,imageContrast,useColor)

timeOffset = getappdata(params.hFig,'timeOffset');
callOffset = getappdata(params.hFig,'callOffset');

[~,first_call_frame_idx] = min(abs(frame_ts_info.(['timestamps_' params.timestamps_string])-call_time));
frame_ts_info_fname = fullfile(frame_ts_info.videoFNames{frame_ts_info.fileIdx(first_call_frame_idx)});
exp_dir_split = strsplit(params.exp_dir,filesep);
frame_ts_info_fname_split = strsplit(frame_ts_info_fname,filesep);
dateIdx = find(ismember(frame_ts_info_fname_split,exp_dir_split),1,'last');
video_fName = fullfile(params.exp_dir,frame_ts_info_fname_split{dateIdx+1:end});

vidObj = VideoReader(video_fName);
video_fs = vidObj.FrameRate;
frame_offset = round(video_fs*callOffset);
time_offset = round(video_fs*timeOffset);

callFrame = frame_ts_info.file_frame_number(first_call_frame_idx);
frameIdx = time_offset + [callFrame-frame_offset callFrame+frame_offset];

if frameIdx(1) <= 0
    disp('Frame index less than 0. Continuing to next event')
    videoData = [];
    video_fs = NaN;
    return
end

frameIdx(2) = min(round(vidObj.Duration*vidObj.FrameRate)-1,frameIdx(2));
nFrames = diff(frameIdx)+1;
video_frame_size = [vidObj.Height,vidObj.Width];
downsampleFactor = 2;
if video_frame_size(1) > 1e4
    downsampleFlag = true;
    video_frame_size = floor(video_frame_size/downsampleFactor);
else
    downsampleFlag = false;
end

frameData = zeros(video_frame_size(1),video_frame_size(2),3,nFrames,'uint8');

vidObj.CurrentTime = frameIdx(1)/vidObj.FrameRate;
for k = 1:nFrames
    frameData(:,:,:,k) = readFrame(vidObj);
end

if useColor
    vidIdxs = repmat({':'},1,3);
    video_data_size = {video_frame_size(1),video_frame_size(2),3,nFrames};
else
    vidIdxs = repmat({':'},1,2);
    video_data_size = {video_frame_size(1),video_frame_size(2),nFrames};
    frameData = squeeze(frameData(:,:,1,:));
end

if downsampleFlag
    frameData = frameData(1:downsampleFactor:vidObj.Height,1:downsampleFactor:vidObj.Width,:,:);
end

if imageContrast > 0
    videoData = zeros(video_data_size{:},'uint8');
    for frame_k = 1:nFrames
        videoData(vidIdxs{:},frame_k) = adapthisteq(frameData(:,:,frame_k),'ClipLimit',imageContrast,'NumTiles',[8 8]);
    end
else
    videoData = frameData;
end

end

function insertButtons(params,audio_dir,event_pos_data,frame_ts_info,allBehaviorList)

call_k = getappdata(params.hFig,'call_k');
call_info = guidata(params.hFig);
playbackSpeed = getappdata(params.hFig,'playbackSpeed');
callOffset = getappdata(params.hFig,'callOffset');
timeOffset = getappdata(params.hFig,'timeOffset');
imageContrast = getappdata(params.hFig,'imageContrast');
nBehaviors = length(call_info(call_k).behaviors);

% Play button with text Start/Pause/Continue

controlPanel = uipanel(params.hFig,'unit','normalized','Title','Control Panel',...
    'Position',[0.05 0.91 0.45 0.075],'Tag','controls');

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Start',...
    'position',[0.01 0.1 0.1 0.9],'Tag','startButton',...
    'callback',{@playCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Stop',...
    'position',[0.13 0.1 0.1 0.9],'Tag','stopButton',...
    'callback',{@playCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Seek',...
    'position',[0.25 0.1 0.1 0.9],'Tag','seekButton',...
    'callback',{@seekCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Next',...
    'position',[0.37 0.1 0.1 0.9],'Tag','loadNextFile','callback', ...
    {@nextVideoCallback,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Return Data',...
    'position',[0.49 0.1 0.1 0.9],'callback',{@returnDataCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Save Data',...
    'position',[0.61 0.1 0.1 0.9],'callback',{@saveDataCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Load Data',...
    'position',[0.73 0.1 0.1 0.9],'callback',{@loadDataCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Save Video',...
    'position',[0.85 0.1 0.1 0.9],'callback',{@save_video_callback,params});



vocalizationPanel = uipanel(params.hFig,'unit','normalized','Title','Vocalization Panel',...
    'Position',[0.02 0.5 0.076 0.35],'Tag','vocalization');

involvedString = call_info(call_k).batsInvolved;

if ~isempty(involvedString)
    if length(involvedString) == 1
        involvedString = [involvedString{:}];
    end
    involvedValue = find(ismember([{''} params.bat_IDs],involvedString));
else
    involvedValue = 1;
end

if isempty(involvedValue)
    involvedValue = 1;
end
uicontrol(vocalizationPanel,'unit','normalized','style','listbox','string',...
        [{''} params.bat_IDs],'position',[0.01 0.4 0.9 0.5],'value',involvedValue,...
        'Min',0,'Max',length(params.bat_IDs),'callback',{@updateCallInfoCallback,params,call_k});

if ~isnan(event_pos_data(call_k).batNum)
    voc_bat_ID = event_pos_data(call_k).batNum;
else
    voc_bat_ID = '?';
end
    
uicontrol(vocalizationPanel,'unit','normalized','style','text','string',...
    ['Vocalizer: ' voc_bat_ID],'position',[0.01 0.2 0.9 0.2]);

callNumbers = strsplit(num2str(1:length(event_pos_data)));
eventpos = num2cell(round(params.eventpos),1);

uicontrol(params.hFig,'unit','normalized','style','popupmenu','string',...
    cellfun(@(x,y) [x ': ' num2str(mean(y)) ' min'],callNumbers,eventpos,'UniformOutput',0),...
    'position',[0.02,0.4,0.05,0.05],'Tag','loadNextAudioFile',...
    'value',call_k,'callback',...
    {@nextVideoCallback,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList});

playbackPanel = uipanel(params.hFig,'unit','normalized','Title','Playback Panel',...
    'Position',[0.51 0.91 0.45 0.075],'Tag','controls');

uicontrol(playbackPanel,'unit','normalize','style','slider','Min',1,'Max',...
    20,'Value',playbackSpeed,'SliderStep',[0.1 0.2],'position',...
    [0,0,0.15,0.55],'Tag','playbackSpeed','callback',...
    {@updatePlaybackSpeed,params})

uicontrol(playbackPanel,'Style','text','units','normalized','position',...
    [0.01 0.75 0.15 0.25],'Tag','playback_speed_text','String',...
    ['Playback speed: 1/' num2str(round(playbackSpeed*10)/10) 'x']);

uicontrol(playbackPanel,'unit','normalize','style','slider','Min',0.1,'Max',...
    5,'Value',callOffset,'SliderStep',[0.05 0.1],'position',...
    [0.175,0,0.15,0.55],'Tag','callOffset','callback',...
    {@updateCallOffset,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList})

uicontrol(playbackPanel,'Style','text','units','normalized','position',...
    [0.175 0.75 0.15 0.25],'Tag','call_offset_text','String',...
    ['Call offset: ' num2str(round(callOffset*10)/10) ' s']);

uicontrol(playbackPanel,'unit','normalize','style','slider','Min',-2,'Max',...
    2,'Value',timeOffset,'SliderStep',[0.05 0.1],'position',...
    [0.35,0,0.15,0.55],'Tag','timeOffset','callback',...
    {@updateTimeOffset,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList})

uicontrol(playbackPanel,'Style','text','units','normalized','position',...
    [0.35 0.75 0.15 0.25],'Tag','time_offset_text','String',...
    ['Time offset: ' num2str(round(timeOffset*10)/10) ' s']);

uicontrol(playbackPanel,'unit','normalize','style','slider','Min',0,'Max',...
    0.25,'Value',imageContrast,'SliderStep',[0.01 0.1],'position',...
    [0.525,0,0.15,0.55],'Tag','imageContrast','callback',...
    {@updateContrast,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList})

uicontrol(playbackPanel,'Style','text','units','normalized','position',...
    [0.525 0.75 0.15 0.25],'Tag','image_contrast_text','String',...
    ['Image contrast: ' num2str(round(imageContrast*100)/100)]);

% Frame by frame controls
uicontrol(playbackPanel,'unit','normalized','style','pushbutton','string','<<',...
    'position',[0.7 0.1 0.1 0.9],'Tag','jumpBackFrames',...
    'callback',{@playCallback,params});
uicontrol(playbackPanel,'unit','normalized','style','pushbutton','string','<',...
    'position',[0.775 0.1 0.1 0.9],'Tag','prevFrame',...
    'callback',{@playCallback,params});
uicontrol(playbackPanel,'unit','normalized','style','pushbutton','string','>',...
    'position',[0.85 0.1 0.1 0.9],'Tag','nextFrame',...
    'callback',{@playCallback,params});
uicontrol(playbackPanel,'unit','normalized','style','pushbutton','string','>>',...
    'position',[0.925 0.1 0.1 0.9],'Tag','jumpAheadFrames',...
    'callback',{@playCallback,params});

behaviorPanel = uipanel(params.hFig,'unit','normalized','Title',...
    'Behavior Panel','Position',[0.02 0.025 0.15 0.15],'Tag','behavior');

for b = 1:nBehaviors
    position = [0.01 + (1/nBehaviors)*(b-1),0.05,(1/nBehaviors)-0.01,0.9];
    
    subBehaviorPanel = uipanel(behaviorPanel,'unit','normalized','Title',...
        ['Behavior #' num2str(b)],'Position',position,'Tag','behavior',...
        'UserData',b);
    
    behaviorString = call_info(call_k).behaviors{b};
    if ~isempty(behaviorString)
        behaviorStringSplit = strsplit(behaviorString,'-');
        behaviorValue = find(strcmp(behaviorStringSplit{2}, [{''} allBehaviorList]));
        batIdentityValues = strcmp(behaviorStringSplit{1},{'Grouped','Spread'});
    else
        behaviorValue = 1;
        batIdentityValues = zeros(1,2);
    end

    bhvTime = round(call_info(call_k).behaviorTime{b} - call_info(call_k).corrected_eventpos(1));
    uicontrol(subBehaviorPanel,'unit','normalized','style','pushbutton','string',...
        ['Time relative to call: ' num2str(bhvTime) 'ms'],'position',[0.01 0.6 0.9 0.3],...
        'callback',{@updateCallInfoCallback,params,call_k},'Tag','behaviorTime');
    
    uicontrol(subBehaviorPanel,'unit','normalized','style','popupmenu','string',...
        [{''} allBehaviorList],'position',[0.01 0.1 0.9 0.15],'value',behaviorValue,...
        'callback',{@updateCallInfoCallback,params,call_k},'Tag','bhvType');
    
    bgGrouping = uibuttongroup(subBehaviorPanel,'Position',[0 0.25 1 0.2],...
        'SelectionChangedFcn',{@updateCallInfoCallback,params,call_k},...
        'Tag','bhvGroup');
    uicontrol(bgGrouping,'unit','normalized','Style','radiobutton','String',...
        'Grouped','position',[0 0 0.5 1],'value',batIdentityValues(1));
    uicontrol(bgGrouping,'unit','normalized','Style','radiobutton','String',...
        'Spread','position',[0.5 0 0.5 1],'value',batIdentityValues(2));
    
end


end

function playCallback(hObject,~,params)

a = getappdata(params.hFig,'a');
currentFrame = getappdata(params.hFig,'currentFrame');
currentSample = max(1,round((currentFrame - 1) * (params.audio_fs/params.video_fs)));

switch hObject.String
    
    case {'>','>>','<','<<'}
        switch hObject.String
            case '>'
                currentFrame = currentFrame + 1;
            case '>>'
                currentFrame = currentFrame + 5;
            case '<'
                currentFrame = max(currentFrame - 1,1);
            case '<<'
                currentFrame = max(currentFrame - 5,1);
        end
        
        plot_frame_step(currentFrame,a.timerFc{2:end},currentSample)
        a.UserData = currentFrame;
        setappdata(params.hFig,'currentFrame',currentFrame)
        setappdata(params.hFig,'a',a);
    case 'Start'
        setappdata(params.hFig,'isPlayingVideo',1)
        delete(findobj('Tag','audioMarker'));
        plot(params.hAudioTrace,[currentSample currentSample], [-1 1],'k','Tag','audioMarker');
        hObject.String = 'Pause';
        a.play(currentSample);
    case 'Continue'
        hObject.String = 'Pause';
        a.resume
        delete(findobj('Tag','audioMarker'));
        plot(params.hAudioTrace,[currentSample currentSample], [-1 1],'k','Tag','audioMarker');
    case 'Pause'
        hObject.String = 'Continue';
        setappdata(params.hFig,'isPlayingVideo',0)
        currentFrame = round(a.CurrentSample * (params.video_fs/params.audio_fs));
        setappdata(params.hFig,'currentFrame',currentFrame)
        a.pause
    case 'Stop'
        setappdata(params.hFig,'currentFrame',1)
        setappdata(params.hFig,'isPlayingVideo',0)
        set(findobj('Tag','startButton'),'String','Start');
        a.pause;
        a.UserData = 1;
        setappdata(params.hFig,'startAudio',0)
        setappdata(params.hFig,'a',a);
        delete(findobj('Tag','audioMarker'));
end

% When video reaches the end of file, display "Start" on the
% play button.
if currentFrame >= params.nFrames && ~strcmp(hObject.String,{'>','>>','<','<<'})
    try
        hObject.String = 'Start';
    catch err
        switch err.identifier
            case 'MATLAB:class:InvalidHandle'
                
            otherwise
                rethrow(err);
        end
        
    end
    
    setappdata(params.hFig,'isPlayingVideo',0)
    setappdata(params.hFig,'startAudio',0)
end
end

function nextVideoCallback(hObject,~,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

switch hObject.Tag
    case 'loadNextFile'
        call_k = call_k + 1;
    case 'loadNextAudioFile'
        call_k = hObject.Value;
    case 'callOffset'
        % pass
end
setappdata(params.hFig,'call_k',call_k)
set(params.hFig, 'pointer', 'watch')
drawnow();

[audioData,videoData,params,success] = loadNextCall(event_pos_data,frame_ts_info,params);
if success
    hStartButton = findobj(params.hFig.Children,'Tag','startButton');
    set(hStartButton,'String','Start');
    initMovie(videoData,audioData,params,audio_dir,event_pos_data,frame_ts_info,allBehaviorList);
else
    disp('couldn''t load requested audio file');
end


set(params.hFig, 'pointer', 'arrow')

end

function updateCallInfoCallback(hObject,~,params,call_k)

call_info = guidata(params.hFig);

switch hObject.Parent.Tag
    
    case 'vocalization'
        
        if length(hObject.Value) > 1
            call_info(call_k).batsInvolved = hObject.String(hObject.Value);
        else
            if strcmp(hObject.String(hObject.Value),'')
                call_info(call_k).batsInvolved = [];
            else
                call_info(call_k).batsInvolved = {hObject.String(hObject.Value)};
            end
        end
        
    case 'behavior'
        behaviorNum = hObject.Parent.UserData;
        behaviorUIObjs = hObject.Parent.Children;
        behaviorString = '';
        behavior_str_component_tags = {'bhvType','bhvGroup'};
        for bhvTags = behavior_str_component_tags
            obj = findobj(hObject.Parent,'Tag',bhvTags{1});
            if isempty(obj)
                continue
            end
            switch bhvTags{1}
                case 'bhvGroup'
                    behaviorString = [obj.SelectedObject.String '-' behaviorString];
                    
                case 'bhvType'
                    if ~isempty(obj.String{obj.Value})
                        behaviorString = [behaviorString obj.String{obj.Value}];
                    else
                        behaviorString = '';
                        break
                    end
                    
            end
        end
        
        call_info(call_k).behaviors{behaviorNum} = behaviorString;
        
        if strcmp(hObject.Tag,'behaviorTime')
            currentFrame = getappdata(params.hFig,'currentFrame');
            callOffset = 1e3*getappdata(params.hFig,'callOffset');
            call_info(call_k).behaviorTime{behaviorNum} = call_info(call_k).corrected_eventpos(1) - callOffset + 1e3*(currentFrame/params.video_fs);
            bhvTime = round(call_info(call_k).behaviorTime{behaviorNum} - call_info(call_k).corrected_eventpos(1));
            hObject.String = ['Time relative to call: ' num2str(bhvTime) 'ms'];
        end
        
end

guidata(params.hFig,call_info);
end

function returnDataCallback(~,~,params)
call_info = guidata(params.hFig);
assignin('base','call_info',call_info)
end

function loadDataCallback(~,~,params)
call_info_fname = params.call_info_fname;

try
    load(call_info_fname,'call_info');
    display(['loading existing file ' call_info_fname]);
    guidata(params.hFig,call_info);
    
catch
    try
        [call_info_fname, folder] = uigetfile(fullfile(fileparts(call_info_fname),'*.mat'));
        load(fullfile(folder,call_info_fname),'call_info')
        guidata(params.hFig,call_info);
    catch
        display(['couldn''t find' call_info_fname]);
    end
end

if ~isfield(call_info,'behaviorTime')
    nBehaviors = length(call_info(1).behaviors);
    [call_info.behaviorTime] = deal(cell(1,nBehaviors));
    guidata(params.hFig,call_info);
end

end

function saveDataCallback(~,~,params)
call_info_fname = params.call_info_fname;

if exist(call_info_fname,'file')
    display(['updating existing file ' call_info_fname]);
    choice = questdlg('Overwrite existing juvenile call file?','Overwrite?','Yes','No','Update','No');
    switch choice
        case 'Yes'
            call_info = guidata(params.hFig);
            save(call_info_fname,'call_info');
        case 'Update'
            call_info_update = guidata(params.hFig);
            s = load(call_info_fname);
            call_info = s.call_info;
            call_info = updateData(call_info,call_info_update);
            save(call_info_fname,'call_info');
    end
else
    display(['saving new file ' call_info_fname]);
    call_info = guidata(params.hFig);
    save(call_info_fname,'call_info');
end

end

function call_info = updateData(call_info,call_info_update)

nFiles = length(call_info);
nUpdates = length(call_info_update);

for up = 1:nUpdates
    idx = strcmp(call_info_update(up).AudioFile, {call_info.AudioFile});
    call_info(idx) = call_info_update(up);
end

end

function updatePlaybackSpeed(hObject,~,params)

a = getappdata(params.hFig,'a');

if ~(a.isplaying)
    a.SampleRate = params.audio_fs/hObject.Value;
    a.TimerPeriod = hObject.Value/params.video_fs;
    setappdata(params.hFig,'playbackSpeed',hObject.Value);
    setappdata(params.hFig,'a',a);
    
    textH = findobj(params.hFig,'Tag','playback_speed_text');
    textH.String = ['Playback speed: 1/' num2str(round(hObject.Value*10)/10) 'x'];
    
else
    hObject.Value = getappdata(params.hFig,'playbackSpeed');
    disp('Cannot update playback speed during playback');
end


end

function updateContrast(hObject,~,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

setappdata(params.hFig,'imageContrast',hObject.Value);
textH = findobj(params.hFig,'Tag','image_contrast_text');
textH.String = ['Image contrast: ' num2str(round(hObject.Value*100)/100)];

nextVideoCallback(hObject,[],params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

end

function updateTimeOffset(hObject,~,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

setappdata(params.hFig,'timeOffset',hObject.Value);
textH = findobj(params.hFig,'Tag','time_offset_text');
textH.String = ['Call offset: ' num2str(round(hObject.Value*10)/10) ' s'];

nextVideoCallback(hObject,[],params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

end

function updateCallOffset(hObject,~,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

setappdata(params.hFig,'callOffset',hObject.Value);
textH = findobj(params.hFig,'Tag','call_offset_text');
textH.String = ['Call offset: ' num2str(round(hObject.Value*10)/10) ' s'];

nextVideoCallback(hObject,[],params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

end

function plot_frame_audioplayer_callback(player,~,videoData,audioAxis,imObj)

for v = 1:length(videoData)
    frame_k = min(size(videoData{v},ndims(videoData{v})),player.UserData);
    if ~isempty(videoData{v})
        vidIdxs = repmat({':'},1,ndims(videoData{v})-1);
        set(imObj{v}, 'CData', videoData{v}(vidIdxs{:},frame_k));
    end
end
player.UserData = frame_k + 1;

currentSample = player.CurrentSample;

% plot the new marker
h = findobj(audioAxis,'Tag','audioMarker');
if ~isempty(h)
    h(1).XData = repmat(currentSample,1,2);
else
    plot(audioAxis,repmat(currentSample,1,2), [-1 1],'k','Tag','audioMarker');
end

end

function plot_frame_step(frame_k,videoData,audioAxis,imObj,current_audio_sample)

for v = 1:length(videoData)
    frame_k = min(size(videoData{v},ndims(videoData{v})),frame_k);
    if ~isempty(videoData{v})
        vidIdxs = repmat({':'},1,ndims(videoData{v})-1);
        set(imObj{v}, 'CData', videoData{v}(vidIdxs{:},frame_k));
    end
end
% plot the new marker
h = findobj(audioAxis,'Tag','audioMarker');
if ~isempty(h)
    h(1).XData = repmat(current_audio_sample,1,2);
else
    plot(audioAxis,repmat(current_audio_sample,1,2), [-1 1],'k','Tag','audioMarker');
end

end

function stop_video_playback(obj,~,params)

if obj.CurrentSample == 1
    setappdata(params.hFig,'currentFrame',1);
    setappdata(params.hFig,'isPlayingVideo',0)
    set(findobj('Tag','startButton'),'String','Start');
    setappdata(params.hFig,'startAudio',0)
    delete(findobj('Tag','audioMarker'));
    obj.UserData = 1;
end

end

function seekCallback(~,~,params)

axis(params.hAudioTrace);
[x_pos,~] = ginput(1);
currentFrame = round(x_pos * (params.video_fs/params.audio_fs));
setappdata(params.hFig,'currentFrame',currentFrame);

a = getappdata(params.hFig,'a');
a.UserData = currentFrame;
setappdata(params.hFig,'a',a);

end

function save_video_callback(~,~,params)

appData = getappdata(params.hFig);
video_clip_dir = params.exp_dir;
exp_dir_file_parts = strsplit(params.exp_dir,'\');

for v_k = 1:params.nVideo
    video_clip_fname = ['clip_' exp_dir_file_parts{end} '_call_' num2str(appData.call_k) '_camera_' num2str(v_k) '.mp4'];
    video_clip_fname = fullfile(video_clip_dir,video_clip_fname);
    videoData = appData.a.TimerFcn{2}{v_k};
    v = VideoWriter(video_clip_fname,'MPEG-4');
    v.FrameRate = round(params.video_fs/appData.playbackSpeed);
    open(v)
    for frame_k = 1:size(videoData,3)
        writeVideo(v,videoData(:,:,frame_k));
    end
    close(v)
    
    fprintf('Video clip saved to %s\n',video_clip_fname);
end

end