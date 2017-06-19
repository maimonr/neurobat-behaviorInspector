function inspect_audio_video(bat_str,exp_date,exp_dir,varargin)
nVideo = 2;
h = figure;
nBehaviors = 8;

allBehaviorList = {'Bite','Shiver','Survey','L2F','Climb','Claw','Voc','Flap',...
    'E','uFall','Sniff','Wrist','Spread','LG','Strike','M2B','nE'};

hAudioTrace = axes('Units','Normalized','Position',[0.13 0.175 0.775 0.15]);
hMovie = [axes('Units','Normalized','Position',[0.15 0.33 0.335 0.56]),...
    axes('Units','Normalized','Position',[0.575 0.33 0.335 0.56])];


fileNums = struct('video',1,'audio',1);

playParams = struct('audio_fs',250e3,'video_fs',[],'audioVideoOffset',[],...
    'nFrames',[],'nextFile',1);

plotParams = struct('hAudioTrace',hAudioTrace,'hMovie',hMovie,'hFig',h,...
    'bat_str',bat_str,'exp_date',exp_date);

setappdata(plotParams.hFig,'currentFrame',1);
setappdata(plotParams.hFig,'startAudio',0);
setappdata(plotParams.hFig,'endAudio',0);
setappdata(plotParams.hFig,'isPlayingVideo',0);
setappdata(plotParams.hFig,'initialize',1);
setappdata(plotParams.hFig,'playbackSpeed',2);

video_files = cell(1,nVideo);
video_metadata_files = cell(1,nVideo);
video_dirs = cell(1,nVideo);

for v = 1:nVideo
    video_dirs{v} = [uigetdir(exp_dir,sprintf('Choose folder for Camera %d',v)) filesep];
    video_files{v} = dir([video_dirs{v} '*.mp4']);
    video_files{v} = {video_files{v}.name};
    video_metadata_files{v} = dir([video_dirs{v} '*.ts.csv']);
    video_metadata_files{v} = {video_metadata_files{v}.name};
end

audio_dir = [uigetdir(exp_dir,'Choose folder containing audio files') filesep];
audio_files = dir([audio_dir '*.wav']);
audio_files = {audio_files.name};

if length(video_files{1}) == length(video_files{2}) &&...
        length(video_metadata_files{1}) == length(video_metadata_files{2}) && ...
        length(video_metadata_files{1}) == length(video_files{2})
else
    display('irregular number of video files');
    keyboard;
end

juv_call_info_fname = [audio_dir 'juv_call_info_' plotParams.bat_str '_' plotParams.exp_date '.mat'];
juv_call_info = struct('AudioFile',audio_files,'juvCall',[],'echoCall',[],'VideoFile',[],'behaviors',{cell(1,nBehaviors)});

if isempty(varargin)
    try
        s = load(juv_call_info_fname);
        juv_call_field_names = fieldnames(s.juv_call_info);
        for f = 1:length(juv_call_field_names)
           if isfield(juv_call_info,juv_call_field_names{f})
               [juv_call_info.(juv_call_field_names{f})] = deal(s.juv_call_info.(juv_call_field_names{f}));
           end
        end
        display(['loading existing file ' juv_call_info_fname]);
        fileNums.audio = find(~cellfun(@isempty,{juv_call_info.VideoFile}),1,'first');
        fileNums.video = find(strcmp(juv_call_info(fileNums.audio).VideoFile{1},video_files{1}));
    catch
        display(['couldn''t find ' juv_call_info_fname]);
    end
else
    s = varargin{1};
    audio_files = {s.AudioFile};
    video_files = cell(1,nVideo);
    for k = 1:length(s)
        for v = 1:nVideo
            video_files{v}{k} = s(k).VideoFile{v};
        end
    end
    juv_call_info.VideoFile = {s.VideoFile};
end

guidata(plotParams.hFig,juv_call_info);

[audioData,videoData,playParams] = loadNextFile(audio_dir,video_dirs,audio_files,video_files,playParams,fileNums);

initMovie(videoData,audioData,playParams,plotParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList);

end

function initMovie(videoData,audioData,playParams,plotParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList)
playbackSpeed = getappdata(plotParams.hFig,'playbackSpeed');
a = audioplayer(audioData,playParams.audio_fs/playbackSpeed);
setappdata(plotParams.hFig,'a',a);

figure(plotParams.hFig);
if getappdata(plotParams.hFig,'initialize');
    setappdata(plotParams.hFig,'initialize',0);
else
    UIPanels = findobj(plotParams.hFig,'type','UIPanel');
    UIButtons = findobj(plotParams.hFig,'type','UIControl');
    delete([UIPanels; UIButtons]);
end

insertButtons(plotParams,videoData,playParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList);

cla(plotParams.hAudioTrace);
plot(plotParams.hAudioTrace,(1:length(audioData))/playParams.audio_fs,audioData)
xlim(plotParams.hAudioTrace,[0 length(audioData)/playParams.audio_fs])
ylim(plotParams.hAudioTrace,[min(audioData) max(audioData)]);
hold(plotParams.hAudioTrace,'on');
axis(plotParams.hAudioTrace,'off');

for s = 1:2
    cla(plotParams.hMovie(s))
    imshow(videoData(:,:,1,s),'Parent',plotParams.hMovie(s));
end

end

function [audioData,videoData,playParams,success] = loadNextFile(audio_dir,video_dirs,audio_files,video_files,playParams,fileNums)

try
    audioFile = [audio_dir audio_files{fileNums.audio}];
    audioData = audioread(audioFile);
    success = 1;
    videoData = cell(1,2);
    for v = 1:2
        videoFile = [video_dirs{v} video_files{v}{fileNums.video}];
        [videoData{v}, frameDateTimesSorted] = importAndSortVideo(videoFile);
    end
catch err
    audioData = [];
    videoData = [];
    playParams = [];
    display(err)
    keyboard
    success = 0;
    return
end


try
    videoData = cat(4,videoData{1},videoData{2});
catch err
    n_frames_by_camera = cellfun(@(x) size(x,3),videoData);
    display(['There is a difference of ' num2str(abs(diff(n_frames_by_camera)))...
        ' frames between Camera 1 and 2. Truncating...']);
    [n_frames,shortVideo] = min(n_frames_by_camera);
    videoData{setdiff(1:2,shortVideo)} = videoData{setdiff(1:2,shortVideo)}...
        (:,:,1:n_frames);
    videoData = cat(4,videoData{1},videoData{2});
    frameDateTimesSorted = frameDateTimesSorted(1:n_frames);
end
playParams.nFrames = length(frameDateTimesSorted);
playParams.video_fs = playParams.nFrames/seconds(range(frameDateTimesSorted));
playParams.audioVideoOffset = playParams.nFrames/playParams.video_fs...
    - length(audioData)/playParams.audio_fs;



end

function insertButtons(plotParams,videoData,playParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList)

juv_call_info = guidata(plotParams.hFig);
playbackSpeed = getappdata(plotParams.hFig,'playbackSpeed');
nBehaviors = length(juv_call_info(fileNums.audio).behaviors);
checkJuv = strcmp(juv_call_info(fileNums.audio).juvCall,'juv');
checkAdult = strcmp(juv_call_info(fileNums.audio).juvCall,'adult');
checkNoise = strcmp(juv_call_info(fileNums.audio).juvCall,'noise');
checkUnclear = strcmp(juv_call_info(fileNums.audio).juvCall,'unclear');
checkJuvEcho = strcmp(juv_call_info(fileNums.audio).echoCall,'juvEcho');
checkAdultEcho = strcmp(juv_call_info(fileNums.audio).echoCall,'adultEcho');
checkUnclearEcho = strcmp(juv_call_info(fileNums.audio).echoCall,'unclearEcho');

% Play button with text Start/Pause/Continue

controlPanel = uipanel(plotParams.hFig,'unit','normalized','Title','Control Panel',...
    'Position',[0.09 0.91 0.3 0.075],'tag','controls');

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Start',...
    'position',[0.05 0.1 0.15 0.9],'tag','startButton',...
    'callback',{@playCallback,videoData,playParams,plotParams});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Next',...
    'position',[0.25 0.1 0.15 0.9],'tag','loadNextFile','callback', ...
    {@nextVideoCallback,playParams,plotParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Return Data',...
    'position',[0.45 0.1 0.15 0.9],'callback',{@returnDataCallback,plotParams});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Save Data',...
    'position',[0.65 0.1 0.15 0.9],'callback',{@saveDataCallback,plotParams,audio_dir});

uicontrol(controlPanel,'unit','normalized','style','pushbutton','string','Load Data',...
    'position',[0.85 0.1 0.15 0.9],'callback',{@loadDataCallback,plotParams,audio_dir});



vocalizationPanel = uipanel(plotParams.hFig,'unit','normalized','Title','Vocalization Panel',...
    'Position',[0.02 0.5 0.076 0.35],'tag','vocalization');

uicontrol(vocalizationPanel,'unit','normalized','style','checkbox','string','Juvenile Vocalization',...
    'position',[0.1,0.9,0.9,0.1],'value',checkJuv,'callback', ...
    {@updateJuvCallCallback,plotParams,fileNums,video_files});

uicontrol(vocalizationPanel,'unit','normalized','style','checkbox','string','Adult Vocalization',...
    'position',[0.1,0.8,0.9,0.1],'value',checkAdult,'callback', ...
    {@updateJuvCallCallback,plotParams,fileNums,video_files});

uicontrol(vocalizationPanel,'unit','normalized','style','checkbox','string','Unclear attribution',...
    'position',[0.1,0.7,0.9,0.1],'value',checkUnclear,'callback', ...
    {@updateJuvCallCallback,plotParams,fileNums,video_files});

uicontrol(vocalizationPanel,'unit','normalized','style','checkbox','string','Juvenile Echo',...
    'position',[0.1,0.5,0.9,0.1],'value',checkJuvEcho,'callback', ...
    {@updateJuvCallCallback,plotParams,fileNums,video_files});

uicontrol(vocalizationPanel,'unit','normalized','style','checkbox','string','Adult Echo',...
    'position',[0.1,0.4,0.9,0.1],'value',checkAdultEcho,'callback', ...
    {@updateJuvCallCallback,plotParams,fileNums,video_files});

uicontrol(vocalizationPanel,'unit','normalized','style','checkbox','string','Unclear Echo',...
    'position',[0.1,0.3,0.9,0.1],'value',checkUnclearEcho,'callback', ...
    {@updateJuvCallCallback,plotParams,fileNums,video_files});

uicontrol(vocalizationPanel,'unit','normalized','style','checkbox','string','Noise',...
    'position',[0.1,0.1,0.9,0.1],'value',checkNoise,'callback', ...
    {@updateJuvCallCallback,plotParams,fileNums,video_files});


audio_file_numbers = strsplit(num2str(1:length(audio_files)));

uicontrol(plotParams.hFig,'unit','normalized','style','popupmenu','string',...
    cellfun(@(x,y) [x ' ' y],audio_file_numbers,audio_files,'UniformOutput',0),...
    'position',[0.02,0.4,0.1,0.05],'tag','loadNextAudioFile',...
    'value',fileNums.audio,'callback',...
    {@nextVideoCallback,playParams,plotParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList});

video_file_numbers = strsplit(num2str(1:length(video_files{1})));

uicontrol(plotParams.hFig,'unit','normalized','style','popupmenu','string',...
    cellfun(@(x,y) [x ' ' y],video_file_numbers,video_files{1},'UniformOutput',0),...
    'position',[0.02,0.35,0.1,0.05],'tag','loadNextVideoFile',...
    'value',fileNums.video,'callback',...
    {@nextVideoCallback,playParams,plotParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList});

uicontrol(plotParams.hFig,'unit','normalized','style','text','String','',...
    'position',[0.02,0.29,0.1,0.05],'tag','audioVideoTimeDiff');

uicontrol(plotParams.hFig,'unit','normalize','style','slider','Min',1,'Max',...
    10,'Value',playbackSpeed,'SliderStep',[0.1 0.2],'position',...
    [0.02,0.2,0.1,0.05],'callback',{@updatePlaybackSpeed,plotParams,playParams})

uicontrol('Style','text','units','normalized','position',...
    [0.02 0.25 0.1 0.025],'String','Playback Speed');

behaviorPanel = uipanel(plotParams.hFig,'unit','normalized','Title',...
    'Behavior Panel','Position',[0.13 0.01 0.775 0.15],'tag','behavior');

for b = 1:nBehaviors
    position = [0.01 + (1/nBehaviors)*(b-1),0.05,(1/nBehaviors)-0.01,0.9];
    
    subBehaviorPanel = uipanel(behaviorPanel,'unit','normalized','Title',...
        ['Behavior #' num2str(b)],'Position',position,'tag','behavior',...
        'UserData',b);
    
    behaviorString = juv_call_info(fileNums.audio).behaviors{b};
    if ~isempty(behaviorString)
        behaviorStringSplit = strsplit(behaviorString,'-');
        behaviorValue = find(strcmp(behaviorStringSplit{4}, [{''} allBehaviorList]));
        batIdentityValues = strcmp(behaviorStringSplit{1},{'Juvenile','Adult'});
        contactValues = strcmp(behaviorStringSplit{2},{'Contact','No Contact'});
        contactDirectionValues = strcmp(behaviorStringSplit{3},{'Facing','Not Facing'});
    else
        behaviorValue = 1;
        batIdentityValues = zeros(1,2);
        contactValues = zeros(1,2);
        contactDirectionValues = zeros(1,2);
    end
    
    uicontrol(subBehaviorPanel,'unit','normalized','style','popupmenu','string',...
        [{''} allBehaviorList],'position',[0.01 0.1 0.9 0.15],'value',behaviorValue,...
        'callback',{@updateJuvCallCallback,plotParams,fileNums,video_files});
    
    bgBatIdentity = uibuttongroup(subBehaviorPanel,'Position',[0 0.25 1 0.2],...
        'SelectionChangedFcn',{@updateJuvCallCallback,plotParams,fileNums,video_files},...
        'tag','behavior');
    uicontrol(bgBatIdentity,'unit','normalized','Style','radiobutton','String',...
        'Juvenile','position',[0 0 0.5 1],'value',batIdentityValues(1));
    uicontrol(bgBatIdentity,'unit','normalized','Style','radiobutton','String',...
        'Adult','position',[0.5 0 0.5 1],'value',batIdentityValues(2));
    
    bgContact = uibuttongroup(subBehaviorPanel,'Position',[0 0.5 1 0.2],...
        'SelectionChangedFcn',{@updateJuvCallCallback,plotParams,fileNums,video_files},...
        'tag','behavior');
    uicontrol(bgContact,'unit','normalized','Style','radiobutton','String',...
        'Contact','position',[0 0 0.5 1],'value',contactValues(1));
    uicontrol(bgContact,'unit','normalized','Style','radiobutton','String',...
        'No Contact','position',[0.5 0 0.5 1],'value',contactValues(2));
    
    bgContactDirection = uibuttongroup(subBehaviorPanel,'Position',[0 0.75 1 0.2],...
        'SelectionChangedFcn',{@updateJuvCallCallback,plotParams,fileNums,video_files},...
        'tag','behavior');
    uicontrol(bgContactDirection,'unit','normalized','Style','radiobutton','String',...
        'Facing','position',[0 0 0.5 1],'value',contactDirectionValues(1));
    uicontrol(bgContactDirection,'unit','normalized','Style','radiobutton','String',...
        'Not Facing','position',[0.5 0 0.5 1],'value',contactDirectionValues(2));
    
    
    
end


end

function playCallback(hObject,~,videoData,playParams,plotParams)

% Check the status of play button
isTextStart = strcmp(hObject.String,'Start');
isTextCont  = strcmp(hObject.String,'Continue');
a = getappdata(plotParams.hFig,'a');
playbackSpeed = getappdata(plotParams.hFig,'playbackSpeed');
if isTextStart
    setappdata(plotParams.hFig,'currentFrame',1)
    setappdata(plotParams.hFig,'isPlayingVideo',1)
    audioMarker = plot(plotParams.hAudioTrace,([1 1]/playParams.video_fs) - playParams.audioVideoOffset, [-1 1],'k','tag','audioMarker');
end
if (isTextStart || isTextCont)
    hObject.String = 'Pause';
else
    hObject.String = 'Continue';
    setappdata(plotParams.hFig,'isPlayingVideo',0)
    if getappdata(plotParams.hFig,'startAudio')
        a.pause;
    end
end
currentFrame = getappdata(plotParams.hFig,'currentFrame');
startAudio = getappdata(plotParams.hFig,'startAudio');
if isTextCont
    if getappdata(plotParams.hFig,'startAudio') && a.CurrentSample~=1
        a.resume
    end
    delete(findobj('tag','audioMarker'));
    audioMarker = plot(plotParams.hAudioTrace,([currentFrame currentFrame]/playParams.video_fs) - playParams.audioVideoOffset, [-1 1],'k','tag','audioMarker');
end

while true 
    t1 = tic;
    if ~isvalid(hObject)
        currentFrame = Inf;
        break
    elseif ~strcmp(hObject.String, 'Pause') || currentFrame >= playParams.nFrames
        break
    else
        if (playParams.audioVideoOffset<0) && ~startAudio
            a.play;
            startAudio = 1;
            setappdata(plotParams.hFig,'startAudio',1);
            pause(abs(playParams.audioVideoOffset));
        elseif (currentFrame/playParams.video_fs) >= playParams.audioVideoOffset && ~startAudio
            a.play;
            startAudio = 1;
            setappdata(plotParams.hFig,'startAudio',1);
        end
        for s = 1:2
            image(plotParams.hMovie(s),videoData(:,:,currentFrame,s));
        end
        audioMarker.XData = ([currentFrame currentFrame]/playParams.video_fs) - playParams.audioVideoOffset;
        offset = toc(t1);
        pause((playbackSpeed/playParams.video_fs) - offset);
        currentFrame = currentFrame + 1;
    end
end

setappdata(plotParams.hFig,'currentFrame',currentFrame)

% When video reaches the end of file, display "Start" on the
% play button.
if currentFrame >= playParams.nFrames
    try
        hObject.String = 'Start';
    catch err
        switch err.identifier
            case 'MATLAB:class:InvalidHandle'
                
            otherwise
                rethrow(err);
        end
            
    end
    
    setappdata(plotParams.hFig,'isPlayingVideo',0)
    setappdata(plotParams.hFig,'startAudio',0)
end
end

function nextVideoCallback(hObject,~,playParams,plotParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList)

switch hObject.Tag
    case 'loadNextFile'
        fileNums = structfun(@(x) x+1,fileNums,'UniformOutput',0);
    case 'loadNextAudioFile'
        fileNums.audio = hObject.Value;
    case 'loadNextVideoFile'
        fileNums.video = hObject.Value;
end

set(plotParams.hFig, 'pointer', 'watch')
drawnow;

[audioData,videoData,playParams,success] = loadNextFile(audio_dir,video_dirs,audio_files,video_files,playParams,fileNums);
if success
    hStartButton = findobj(plotParams.hFig.Children,'tag','startButton');
    set(hStartButton,'String','Start');
    initMovie(videoData,audioData,playParams,plotParams,audio_dir,video_dirs,audio_files,video_files,fileNums,allBehaviorList);

    hAudioFileList = findobj(plotParams.hFig.Children,'tag','loadNextAudioFile');
    hVideoFileList = findobj(plotParams.hFig.Children,'tag','loadNextVideoFile');
    
    set(hAudioFileList,'Value',fileNums.audio);
    set(hVideoFileList,'Value',fileNums.video);
    
    audioTime = datetime(audio_files{hAudioFileList.Value}(4:13),'InputFormat','MMddHHmmss');
    videoTime = datetime(video_files{1}{hVideoFileList.Value}(1:end-4),'InputFormat','eee MMM dd HH-mm-ss.SSS');

    timeDiff = audioTime - videoTime;
    
    timeDiffString = ['time difference of: ' char(timeDiff)]; 
    hAudioVideoTimeDiff = findobj(plotParams.hFig,'tag','audioVideoTimeDiff');
    set(hAudioVideoTimeDiff,'String',timeDiffString);
        
else
    display('couldn''t load requested audio file');
end


set(plotParams.hFig, 'pointer', 'arrow')

end

function updateJuvCallCallback(hObject,~,plotParams,fileNums,video_files)

juv_call_info = guidata(plotParams.hFig);

switch hObject.Parent.Tag
    case 'vocalization'
        
        checkValue = get(hObject,'Value');
        maxValue = get(hObject,'Max');
        minValue = get(hObject,'Min');
        
        switch hObject.String
            case 'Juvenile Vocalization'
                if(checkValue == maxValue)
                    display('Recorded as Juvenile Call');
                    juv_call_info(fileNums.audio).juvCall = 'juv';
                elseif(checkValue == minValue)
                    display('Recorded as not Juvenile Call');
                    juv_call_info(fileNums.audio).juvCall = [];
                end
                
            case 'Noise'
                if(checkValue == maxValue)
                    display('Recorded as Noise');
                    juv_call_info(fileNums.audio).juvCall = 'noise';
                elseif(checkValue == minValue)
                    display('Recorded as not Noise');
                    juv_call_info(fileNums.audio).juvCall = [];
                end
                
            case 'Adult Vocalization'
                if(checkValue == maxValue)
                    display('Recorded as Adult Call');
                    juv_call_info(fileNums.audio).juvCall = 'adult';
                elseif(checkValue == minValue)
                    display('Recorded as not Adult');
                    juv_call_info(fileNums.audio).juvCall = [];
                end
                
            case 'Unclear attribution'
                if(checkValue == maxValue)
                    display('Recorded as Unclear Call');
                    juv_call_info(fileNums.audio).juvCall = 'unclear';
                elseif(checkValue == minValue)
                    display('Recorded as not Unclear');
                    juv_call_info(fileNums.audio).juvCall = [];
                end
                
            case 'Juvenile Echo'
                if(checkValue == maxValue)
                    display('Recorded as Juvenile Echo');
                    juv_call_info(fileNums.audio).echoCall = 'juvEcho';
                elseif(checkValue == minValue)
                    display('Recorded as not Juvenile Echo');
                    juv_call_info(fileNums.audio).echoCall = [];
                end
                
            case 'Adult Echo'
                if(checkValue == maxValue)
                    display('Recorded as Adult Echo');
                    juv_call_info(fileNums.audio).echoCall = 'adultEcho';
                elseif(checkValue == minValue)
                    display('Recorded as not Adult Echo');
                    juv_call_info(fileNums.audio).echoCall = [];
                end
                
            case 'Unclear Echo'
                if(checkValue == maxValue)
                    display('Recorded as Unclear Echo');
                    juv_call_info(fileNums.audio).echoCall = 'unclearEcho';
                elseif(checkValue == minValue)
                    display('Recorded as not Unclear Echo');
                    juv_call_info(fileNums.audio).echoCall = [];
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
        
        juv_call_info(fileNums.audio).behaviors{behaviorNum} = behaviorString;
        
end

juv_call_info(fileNums.audio).VideoFile = cellfun(@(x) x(fileNums.video),video_files);

guidata(plotParams.hFig,juv_call_info);
end

function returnDataCallback(~,~,plotParams)

juv_call_info = guidata(plotParams.hFig);
assignin('base','juv_call_info',juv_call_info)
end

function loadDataCallback(~,~,plotParams,audio_dir)

juv_call_info_fname = [audio_dir 'juv_call_info_' plotParams.bat_str '_' plotParams.exp_date '.mat'];

try
    load(juv_call_info_fname);
    display(['loading existing file ' juv_call_info_fname]);
    guidata(plotParams.hFig,juv_call_info);
    
catch
    display(['couldn''t find' juv_call_info_fname]);
end



end

function saveDataCallback(~,~,plotParams,audio_dir)

juv_call_info_fname = [audio_dir 'juv_call_info_' plotParams.bat_str '_' plotParams.exp_date '.mat'];
if exist(juv_call_info_fname,'file')
    display(['updating existing file ' juv_call_info_fname]);
    choice = questdlg('Overwrite existing juvenile call file?','Overwrite?','Yes','No','Update','No');
    switch choice
        case 'Yes'
            juv_call_info = guidata(plotParams.hFig);
            save(juv_call_info_fname,'juv_call_info');
        case 'Update'
            juv_call_info_update = guidata(plotParams.hFig);
            load(juv_call_info_fname);
            juv_call_info = updateData(juv_call_info,juv_call_info_update);
            save(juv_call_info_fname,'juv_call_info');
    end
else
    display(['saving new file ' juv_call_info_fname]);
    juv_call_info = guidata(plotParams.hFig);
    save(juv_call_info_fname,'juv_call_info');
end

end

function juv_call_info = updateData(juv_call_info,juv_call_info_update)

nFiles = length(juv_call_info);
nUpdates = length(juv_call_info_update);

for up = 1:nUpdates
   idx = strcmp(juv_call_info_update(up).AudioFile, {juv_call_info.AudioFile});
   juv_call_info(idx) = juv_call_info_update(up);
end



end

function updatePlaybackSpeed(hObject,~,plotParams,playParams)

a = getappdata(plotParams.hFig,'a');

if ~(a.isplaying || getappdata(plotParams.hFig,'isPlayingVideo'))
    a.SampleRate = playParams.audio_fs/hObject.Value;
    setappdata(plotParams.hFig,'playbackSpeed',hObject.Value);
    setappdata(plotParams.hFig,'a',a);
else
    hObject.Value = getappdata(plotParams.hFig,'playbackSpeed');
    display('Cannot update playback speed during playback');
end


end

