function inspect_audio_video_continuous(exp_dir,varargin)

pnames = {'exp_type','bat_str','exp_date','event_pos_data','onlyBouts'};
dflts  = {'adult','bat',date,[],true};
[exp_type,bat_str,exp_date,event_pos_data,only_bout_flag] = internal.stats.parseArgs(pnames,dflts,varargin{:});

nVideo = 2;
h = figure;
call_k = 1;

hAudioTrace = axes('Units','Normalized','Position',[0.13 0.175 0.775 0.15]);
hMovie = [axes('Units','Normalized','Position',[0.15 0.33 0.335 0.56]),...
    axes('Units','Normalized','Position',[0.575 0.33 0.335 0.56])];
hCalls = axes('Units','Normalized','Position',[0.2 0.075 0.65 0.075]);

params = struct('audio_fs',250e3,'video_fs',[],'nFrames',[],'nVideo',nVideo,...
    'hAudioTrace',hAudioTrace,'hMovie',hMovie,'hFig',h,'hCalls',hCalls,...
    'bat_str',bat_str,'exp_date',exp_date,'exp_type',exp_type,'exp_dir',exp_dir,...
    'eventpos',[],'timestamps_string',[]);

setappdata(params.hFig,'currentFrame',1);
setappdata(params.hFig,'startAudio',0);
setappdata(params.hFig,'endAudio',0);
setappdata(params.hFig,'isPlayingVideo',0);
setappdata(params.hFig,'initialize',1);
setappdata(params.hFig,'playbackSpeed',2);
setappdata(params.hFig,'callOffset',1);
setappdata(params.hFig,'imageContrast',0.01);
setappdata(params.hFig,'call_k',call_k)

video_files = cell(1,nVideo);
video_metadata_files = cell(1,nVideo);
video_dirs = cell(1,nVideo);
frame_ts_info = cell(1,nVideo);
for v = 1:nVideo
    
    video_dirs{v} = fullfile(exp_dir, 'video');
    video_files{v} = dir(fullfile(video_dirs{v},'**',['Camera ' num2str(v) '*.mp4']));
    video_files{v} = {video_files{v}.name};
    video_metadata_files{v} = dir(fullfile(video_dirs{v},'**',['Camera ' num2str(v) '*.ts.csv']));
    video_metadata_files{v} = {video_metadata_files{v}.name};
    frame_ts_fname = dir(fullfile(video_dirs{v},'**','*frame_timestamps_info.mat'));
    frame_ts_fname = frame_ts_fname(contains({frame_ts_fname.name},'camera','IgnoreCase',false) & contains({frame_ts_fname.name},num2str(v)));
    assert(length(frame_ts_fname) == 1);
    s = load(fullfile(frame_ts_fname.folder,frame_ts_fname.name));
    frame_ts_info{v} = s.frame_ts_info;
end

audio_dir = fullfile(exp_dir, 'audio', 'communication', 'ch1');

if isempty(event_pos_data)
    bat_num_classification_fname = fullfile(audio_dir,'manual_al_classify_batNum.mat');
    s = load(fullfile(audio_dir, 'cut_call_data.mat'));
    event_pos_data = s.cut_call_data;
    
    if ~any(isnan([event_pos_data.noise]))
        event_pos_data = event_pos_data(~[event_pos_data.noise]);
    elseif exist(bat_num_classification_fname,'file')
        s = load(bat_num_classification_fname);
        batNums = s.manual_al_classify_batNum;
        if length(batNums) == length(event_pos_data)
            batIdx = ~strcmp(batNums,'noise');
            event_pos_data = event_pos_data(batIdx);
        end
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
    bout_separation_length = 1e3;
    callPos = vertcat(event_pos_data.corrected_eventpos);
    ICI = [Inf; callPos(2:end,1) - callPos(1:end-1,2)];
    callIdx = ICI > bout_separation_length;
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
        
    case 'adult'
        nBehaviors = 1;
        
        allBehaviorList = {'Aggression','Incidental','Probing','Spontaneous','Unclear','Other'};
        call_info_fname = fullfile(audio_dir, ['call_info_' params.bat_str '_' params.exp_date '.mat']);
        call_info = struct('eventpos',num2cell(vertcat(event_pos_data.file_event_pos),2),...
            'corrected_eventpos',num2cell(vertcat(event_pos_data.corrected_eventpos),2),...
            'batsInvolved',[],'behaviors',repmat({cell(1,nBehaviors)},length(event_pos_data),1));
        params.timestamps_string = 'nlg';
        
        params.eventpos = 1e-3*(vertcat(event_pos_data.corrected_eventpos)- frame_ts_info{v}.timestamps_nlg(1))'/60; % call position in minutes
end

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
    
    try
        assert(all(cellfun(@length,bat_ID_strs) == 5));
    catch
        disp('Bat ID numbers are 5 digit strings')
        close(params.hFig);
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
        imObjs{s} = imshow(videoData{s}(:,:,1),'Parent',params.hMovie(s));
    end
    axis(params.hMovie(s),'tight')
    axis(params.hMovie(s),'square')
end

a.TimerFcn = {@plot_frame_audioplayer_callback, a, videoData, params.hAudioTrace, imObjs};
delete(findobj(params.hCalls,'Tag','CallMarker'));
plot(params.hCalls,mean(params.eventpos(:,call_k)),1.1,'vk','MarkerFaceColor','k','Tag','CallMarker');

end

function [audioData,videoData,params,success] = loadNextCall(event_pos_data,frame_ts_info,params)

call_k = getappdata(params.hFig,'call_k');
callOffset = getappdata(params.hFig,'callOffset');
imageContrast = getappdata(params.hFig,'imageContrast');

try
    
    nVideo = params.nVideo;
    call_time = ((event_pos_data(call_k).corrected_eventpos(1)));
    first_call_frame_idx = zeros(1,nVideo);
    video_fs = zeros(1,nVideo);
    vidObj = cell(1,nVideo);
    videoData = cell(1,nVideo);
    for v = 1:nVideo
        [~,first_call_frame_idx(v)] = min(abs(frame_ts_info{v}.(['timestamps_' params.timestamps_string])-call_time));
        Path2Video =frame_ts_info{v}.videoFNames{frame_ts_info{v}.fileIdx(first_call_frame_idx(v))};
        % Make sure the video path is correct
        if isunix
            Path2Video = strrep(Path2Video, '\', '/');
        end
        % find the date of the experiment
        SepIndices = strfind(params.exp_dir, filesep);
        if SepIndices(end)==length(params.exp_dir)
            FolderDate = params.exp_dir(SepIndices(end-1)+1:end-1);
        else
            FolderDate = params.exp_dir(SepIndices(end)+1:end);
        end
        Path2Video_new = fullfile(params.exp_dir, Path2Video((strfind(Path2Video, FolderDate) + length(FolderDate)):end));
        
        vidObj{v} = VideoReader(Path2Video_new);
        video_fs(v) = vidObj{v}.FrameRate;
        frame_offset = round(video_fs(v)*callOffset);
        startFrame = first_call_frame_idx(v) - frame_offset;
        if startFrame <= 0
           disp('Frame index less than 0. Continuing to next event')
           continue
        end
        try
            vidObj{v}.CurrentTime = frame_ts_info{v}.file_frame_number(startFrame)/video_fs(v);
        catch err
            if strcmp(err.identifier,'MATLAB:set:notLessEqual')
                warning('Current video time greater than video length')
            else
                rethrow(err)
            end
        end
        endTime = frame_ts_info{v}.file_frame_number(first_call_frame_idx(v) + frame_offset)/video_fs(v);
        nFrames = ceil((endTime - vidObj{v}.CurrentTime)*video_fs(v));
        video_frame_size = [vidObj{v}.Height,vidObj{v}.Width];
        downsampleFactor = 2;
        if video_frame_size(1) > 512
            downsampleFlag = true;
            video_frame_size = floor(video_frame_size/downsampleFactor);
        else
            downsampleFlag = false;
        end
        videoData{v} = zeros(video_frame_size(1),video_frame_size(2),nFrames,'uint8');
        k = 1;
        while vidObj{v}.CurrentTime <= frame_ts_info{v}.file_frame_number(first_call_frame_idx(v) + frame_offset)/video_fs(v)
            temp = readFrame(vidObj{v});
            temp = temp(:,:,1);
            if downsampleFlag
                temp = temp(1:downsampleFactor:vidObj{v}.Height,1:downsampleFactor:vidObj{v}.Width);
            end
            if imageContrast > 0
                videoData{v}(:,:,k) = adapthisteq(temp,'ClipLimit',imageContrast,'NumTiles',[8 8]);
            else
                videoData{v}(:,:,k) = temp;
            end
            k = k + 1;
        end
    end
    params.video_fs = mean(video_fs);
    params.nFrames = min(cellfun(@(x) size(x,3),videoData));
    if isunix
        Path2Audio_temp = fileparts(strrep(event_pos_data(call_k).fName, '\','/'));
        Path2Audio = fullfile(params.exp_dir, Path2Audio_temp((strfind(Path2Audio_temp, FolderDate) + length(FolderDate)+1):end));
    else
        Path2Audio = fileparts(event_pos_data(call_k).fName);
    end
    Path2Audio_new = fullfile(params.exp_dir, Path2Audio((strfind(Path2Audio, FolderDate) + length(FolderDate)):end));
    audio_files = dir([Path2Audio_new filesep 'T*.WAV']);
    wav_file_nums = cellfun(@(x) str2double(x(end-7:end-4)),{audio_files.name});
    
    audio_offset = round(callOffset*params.audio_fs);
    requested_audio_samples = -audio_offset + event_pos_data(call_k).file_event_pos(1):event_pos_data(call_k).file_event_pos(1) + audio_offset;
    if isunix
        [~, File_local, ext] = fileparts(strrep(event_pos_data(call_k).fName, '\', '/'));
    else
        [~, File_local, ext] = fileparts(event_pos_data(call_k).fName);
    end
    base_audio_data = audioread(fullfile(Path2Audio_new, [File_local ext]));
    
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
    return
end

success = 1;

end

function insertButtons(params,audio_dir,event_pos_data,frame_ts_info,allBehaviorList)

call_k = getappdata(params.hFig,'call_k');
call_info = guidata(params.hFig);
playbackSpeed = getappdata(params.hFig,'playbackSpeed');
callOffset = getappdata(params.hFig,'callOffset');
imageContrast = getappdata(params.hFig,'imageContrast');
nBehaviors = length(call_info(call_k).behaviors);

% Play button with text Start/Pause/Continue

controlPanel = uipanel(params.hFig,'unit','normalized','Title','Control Panel',...
    'Position',[0.05 0.91 0.45 0.075],'tag','controls');

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Start',...
    'position',[0.01 0.1 0.1 0.9],'tag','startButton',...
    'callback',{@playCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Stop',...
    'position',[0.13 0.1 0.1 0.9],'tag','stopButton',...
    'callback',{@playCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Seek',...
    'position',[0.25 0.1 0.1 0.9],'tag','seekButton',...
    'callback',{@seekCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Next',...
    'position',[0.37 0.1 0.1 0.9],'tag','loadNextFile','callback', ...
    {@nextVideoCallback,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Return Data',...
    'position',[0.49 0.1 0.1 0.9],'callback',{@returnDataCallback,params});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Save Data',...
    'position',[0.61 0.1 0.1 0.9],'callback',{@saveDataCallback,params,audio_dir});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Load Data',...
    'position',[0.73 0.1 0.1 0.9],'callback',{@loadDataCallback,params,audio_dir});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Save Video',...
    'position',[0.85 0.1 0.1 0.9],'callback',{@save_video_callback,params});



vocalizationPanel = uipanel(params.hFig,'unit','normalized','Title','Vocalization Panel',...
    'Position',[0.02 0.5 0.076 0.35],'tag','vocalization');

involvedString = call_info(call_k).batsInvolved;
if ~isempty(involvedString)
    involvedValue = find(strcmp(involvedString,[{''} params.bat_IDs]));
else
    involvedValue = 1;
end

if isempty(involvedValue)
    involvedValue = 1;
end
uicontrol(vocalizationPanel,'unit','normalized','style','listbox','string',...
        [{''} params.bat_IDs],'position',[0.01 0.4 0.9 0.5],'value',involvedValue,...
        'Min',0,'Max',length(params.bat_IDs),'callback',{@updateCallInfoCallback,params,call_k});


callNumbers = strsplit(num2str(1:length(event_pos_data)));
eventpos = num2cell(round(params.eventpos),1);

uicontrol(params.hFig,'unit','normalized','style','popupmenu','string',...
    cellfun(@(x,y) [x ': ' num2str(mean(y)) ' min'],callNumbers,eventpos,'UniformOutput',0),...
    'position',[0.02,0.4,0.05,0.05],'tag','loadNextAudioFile',...
    'value',call_k,'callback',...
    {@nextVideoCallback,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList});

uicontrol(params.hFig,'unit','normalize','style','slider','Min',1,'Max',...
    20,'Value',playbackSpeed,'SliderStep',[0.1 0.2],'position',...
    [0.525,0.91,0.1,0.05],'tag','playbackSpeed','callback',...
    {@updatePlaybackSpeed,params})

uicontrol('Style','text','units','normalized','position',...
    [0.525 0.965 0.1 0.025],'tag','playback_speed_text','String',...
    ['Playback speed: 1/' num2str(round(playbackSpeed*10)/10) 'x']);

uicontrol(params.hFig,'unit','normalize','style','slider','Min',0.1,'Max',...
    2,'Value',callOffset,'SliderStep',[0.05 0.1],'position',...
    [0.65,0.91,0.1,0.05],'tag','callOffset','callback',...
    {@updateCallOffset,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList})

uicontrol('Style','text','units','normalized','position',...
    [0.65 0.965 0.1 0.025],'tag','call_offset_text','String',...
    ['Call offset: ' num2str(round(callOffset*10)/10) ' s']);

uicontrol(params.hFig,'unit','normalize','style','slider','Min',0,'Max',...
    0.25,'Value',imageContrast,'SliderStep',[0.01 0.1],'position',...
    [0.8,0.91,0.1,0.05],'tag','imageContrast','callback',...
    {@updateContrast,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList})

uicontrol('Style','text','units','normalized','position',...
    [0.8 0.965 0.1 0.025],'tag','image_contrast_text','String',...
    ['Image contrast: ' num2str(round(imageContrast*100)/100)]);


behaviorPanel = uipanel(params.hFig,'unit','normalized','Title',...
    'Behavior Panel','Position',[0.02 0.025 0.15 0.15],'tag','behavior');

for b = 1:nBehaviors
    position = [0.01 + (1/nBehaviors)*(b-1),0.05,(1/nBehaviors)-0.01,0.9];
    
    subBehaviorPanel = uipanel(behaviorPanel,'unit','normalized','Title',...
        ['Behavior #' num2str(b)],'Position',position,'tag','behavior',...
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
    
    uicontrol(subBehaviorPanel,'unit','normalized','style','popupmenu','string',...
        [{''} allBehaviorList],'position',[0.01 0.1 0.9 0.15],'value',behaviorValue,...
        'callback',{@updateCallInfoCallback,params,call_k});
    
    bgGrouping = uibuttongroup(subBehaviorPanel,'Position',[0 0.25 1 0.2],...
        'SelectionChangedFcn',{@updateCallInfoCallback,params,call_k},...
        'tag','behavior');
    uicontrol(bgGrouping,'unit','normalized','Style','radiobutton','String',...
        'Grouped','position',[0 0 0.5 1],'value',batIdentityValues(1));
    uicontrol(bgGrouping,'unit','normalized','Style','radiobutton','String',...
        'Spread','position',[0.5 0 0.5 1],'value',batIdentityValues(2));
    
end


end

function playCallback(hObject,~,params)

a = getappdata(params.hFig,'a');
currentFrame = getappdata(params.hFig,'currentFrame');
currentSample = round(currentFrame * (params.audio_fs/params.video_fs));

switch hObject.String
    
    case 'Start'
        setappdata(params.hFig,'isPlayingVideo',1)
        plot(params.hAudioTrace,[currentSample currentSample], [-1 1],'k','tag','audioMarker');
        hObject.String = 'Pause';
        a.play(currentSample);
    case 'Continue'
        hObject.String = 'Pause';
        a.resume
        delete(findobj('tag','audioMarker'));
        plot(params.hAudioTrace,[currentSample currentSample], [-1 1],'k','tag','audioMarker');
    case 'Pause'
        hObject.String = 'Continue';
        setappdata(params.hFig,'isPlayingVideo',0)
        a.pause
    case 'Stop'
        setappdata(params.hFig,'currentFrame',1)
        setappdata(params.hFig,'isPlayingVideo',0)
        set(findobj('Tag','startButton'),'String','Start');
        a.pause;
        a.UserData = 1;
        setappdata(params.hFig,'startAudio',0)
        setappdata(params.hFig,'a',a);
        delete(findobj('tag','audioMarker'));
end

% When video reaches the end of file, display "Start" on the
% play button.
if currentFrame >= params.nFrames
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
drawnow;

[audioData,videoData,params,success] = loadNextCall(event_pos_data,frame_ts_info,params);
if success
    hStartButton = findobj(params.hFig.Children,'tag','startButton');
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
        for obj = 1:length(behaviorUIObjs)
            switch behaviorUIObjs(obj).Type
                case 'uibuttongroup'
                    behaviorString = [behaviorUIObjs(obj).SelectedObject.String '-' behaviorString];
                    
                case 'uicontrol'
                    if ~isempty(behaviorUIObjs(obj).String{behaviorUIObjs(obj).Value})
                        behaviorString = [behaviorString behaviorUIObjs(obj).String{behaviorUIObjs(obj).Value}];
                    else
                        behaviorString = '';
                        break
                    end
                    
            end
        end
        
        call_info(call_k).behaviors{behaviorNum} = behaviorString;
        
end

guidata(params.hFig,call_info);
end

function returnDataCallback(~,~,params)

call_info = guidata(params.hFig);
assignin('base','call_info',call_info)
end

function loadDataCallback(~,~,params,audio_dir)

if strcmp(params.exp_type,'nlg')
    call_info_fname = fullfile(audio_dir, ['juv_call_info_' params.bat_str '_' params.exp_date '.mat']);
else
    call_info_fname = fullfile(audio_dir, ['call_info_' params.bat_str '_' params.exp_date '.mat']);
end
try
    load(call_info_fname,'call_info');
    display(['loading existing file ' call_info_fname]);
    guidata(params.hFig,call_info);
    
catch
    try
        [call_info_fname, folder] = uigetfile([audio_dir filesep '*.mat']);
        load([folder call_info_fname],'call_info')
        guidata(params.hFig,call_info);
    catch
        display(['couldn''t find' call_info_fname]);
    end
end



end

function saveDataCallback(~,~,params,audio_dir)

if strcmp(params.exp_type,'nlg')
    call_info_fname = fullfile(audio_dir, ['juv_call_info_' params.bat_str '_' params.exp_date '.mat']);
else
    call_info_fname = fullfile(audio_dir, ['call_info_' params.bat_str '_' params.exp_date '.mat']);
end
if exist(call_info_fname,'file')
    display(['updating existing file ' call_info_fname]);
    choice = questdlg('Overwrite existing juvenile call file?','Overwrite?','Yes','No','Update','No');
    switch choice
        case 'Yes'
            call_info = guidata(params.hFig);
            save(call_info_fname,'call_info');
        case 'Update'
            juv_call_info_update = guidata(params.hFig);
            s = load(call_info_fname);
            call_info = s.call_info;
            call_info = updateData(call_info,juv_call_info_update);
            save(call_info_fname,'call_info');
    end
else
    display(['saving new file ' call_info_fname]);
    call_info = guidata(params.hFig);
    save(call_info_fname,'call_info');
end

end

function call_info = updateData(call_info,juv_call_info_update)

nFiles = length(call_info);
nUpdates = length(juv_call_info_update);

for up = 1:nUpdates
    idx = strcmp(juv_call_info_update(up).AudioFile, {call_info.AudioFile});
    call_info(idx) = juv_call_info_update(up);
end



end

function updatePlaybackSpeed(hObject,~,params)

a = getappdata(params.hFig,'a');

if ~(a.isplaying)
    a.SampleRate = params.audio_fs/hObject.Value;
    a.TimerPeriod = hObject.Value/params.video_fs;
    setappdata(params.hFig,'playbackSpeed',hObject.Value);
    setappdata(params.hFig,'a',a);
    
    textH = findobj(params.hFig,'tag','playback_speed_text');
    textH.String = ['Playback speed: 1/' num2str(round(hObject.Value*10)/10) 'x'];
    
else
    hObject.Value = getappdata(params.hFig,'playbackSpeed');
    disp('Cannot update playback speed during playback');
end


end

function updateContrast(hObject,~,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

setappdata(params.hFig,'imageContrast',hObject.Value);
textH = findobj(params.hFig,'tag','image_contrast_text');
textH.String = ['Image contrast: ' num2str(round(hObject.Value*100)/100)];

nextVideoCallback(hObject,[],params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

end

function updateCallOffset(hObject,~,params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

setappdata(params.hFig,'callOffset',hObject.Value);
textH = findobj(params.hFig,'tag','call_offset_text');
textH.String = ['Call offset: ' num2str(round(hObject.Value*10)/10) ' s'];

nextVideoCallback(hObject,[],params,audio_dir,event_pos_data,frame_ts_info,call_k,allBehaviorList)

end

function plot_frame_audioplayer_callback(~,~,player,videoData,audioMarker,imObj)

for v = 1:length(videoData)
    frame_k = min(size(videoData{v},3),player.UserData);
    if ~isempty(videoData{v})
        set(imObj{v}, 'CData', videoData{v}(:,:,frame_k));
    end
end
player.UserData = frame_k + 1;

x = player.CurrentSample;

% plot the new marker
h = findobj(audioMarker,'Tag','audioMarker');
h.XData = repmat(x,1,2);

end

function stop_video_playback(obj,~,params)

if obj.CurrentSample == 1
    setappdata(params.hFig,'currentFrame',1);
    setappdata(params.hFig,'isPlayingVideo',0)
    set(findobj('Tag','startButton'),'String','Start');
    setappdata(params.hFig,'startAudio',0)
    delete(findobj('tag','audioMarker'));
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
    videoData = appData.a.TimerFcn{3}{v_k};
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