set root=c:\ProgramData\Anaconda3\

call %root%\Scripts\activate.bat %root%

python get_value.py %1 %2 %3 %4
