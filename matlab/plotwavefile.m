% Matlab Script that plots a Wave file

filename = '/Users/olli/cl-wave-file-writer-example-1.wav';
info = audioinfo(filename);

[y,Fs] = audioread(filename);

t = 0:seconds(1/Fs):seconds(info.Duration);
t = t(1:end-1);

figure
for index = 1:info.NumChannels
    subplot(info.NumChannels,1,index);
    plot(t,y(:,index));
    xlabel('t (s)');
    ylabel(strcat('Channel-', num2str(index)));
end


