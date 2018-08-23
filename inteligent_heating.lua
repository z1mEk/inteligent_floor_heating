--[[

Skrypt integligentnie sterujący ogrzewaniem podłogowym
autor: Gabriel Zima
email: gabriel.zima@wp.wp 
data: 2018.03.04
TODO:

]]--

-- funkcja zwracająca różnicę czasu dla daty względem czasu aktualnego.
timedifference = function(s)
    year = string.sub(s, 1, 4);
    month = string.sub(s, 6, 7);
    day = string.sub(s, 9, 10);
    hour = string.sub(s, 12, 13);
    minutes = string.sub(s, 15, 16);
    seconds = string.sub(s, 18, 19);
    t1 = os.time();
    t2 = os.time{year=year, month=month, day=day, hour=hour, min=minutes, sec=seconds};
    difference = os.difftime (t1, t2);
    return difference;
end

-- Zapis do logu
MyLog = function(tekst_log)
    print('Inteli_floor_heating::'..tekst_log);    
end

-- pobranie temperatry z czujników z wilgotnością.
GetTempFromDevice = function(svalue)
    tmp1, tmp2 = svalue:match("([^;]+);([^;]+);([^;]+)");   
    return tonumber(tmp1);
end

commandArray = {}

-- sprawdzenie trybu ogrzewania w celu przewania działania skrytpu
local co_mode = otherdevices['Tryb CO']; -- tryb CO
if (co_mode == 'Lato') then
    --MyLog('Ustawiono tryb Lato.');
    return commandArray;
end

local floor_active = otherdevices['Podłogówka']; -- włączona możliwość ogrzewania podłogówkami.
local ai_heating = otherdevices['Inteligentne ogrzewanie']; -- inteligentne sterowanie ogrzewaniem.
local pompa_parter = 'Off';
local pompa_pietro = 'Off';
local temp_prog_lato = 10;
local temp_desired_normal_day = 20; -- temperatura dzienna zadana gdy wyłączona funkcja grzania podłogówką
local temp_desired_normal_night = 19; -- temperatura nocna zadana gdy wyłączona funkcja grzania podłogówką
local temp_desired_floor = 23; -- temperatura zadana gdy grzane jest podłogówką (takie oszukanie sterownika Vaillant aby nie przestawał grzać wody według swojego pomiaru)
local HC_control = 'On'; -- czy kontrolować krzywą grzewczą. 1 - będzie zmieniana krzywa grzewcza na wyższą gdy wyłączymy podłogówki.
local HC_floor = 0.4; -- krzywa grzewcza dla podłogówki
local HC_radiator = 1.0; -- krzywa grzewcza dla kaloryferów
local HC_current = tonumber(otherdevices_svalues['Krzywa grzewcza']); -- aktualna krzywa grzewcza.
local temp_outside = tonumber(otherdevices_svalues['Temp. zewnętrzna']); -- temperatura zewnętrzna.
local temp_day = temp_desired_normal_day;
local temp_night = temp_desired_normal_night;
local temp_szymek = GetTempFromDevice(otherdevices_svalues['Pokój Szymka']);
local temp_marysia = GetTempFromDevice(otherdevices_svalues['Pokój Marysi']);
local temp_salon = GetTempFromDevice(otherdevices_svalues['Salon']);
local temp_sypialnia = GetTempFromDevice(otherdevices_svalues['Sypialnia']);
local temp_gabinet = GetTempFromDevice(otherdevices_svalues['Gabinet']);
local temp_jadalnia = tonumber(otherdevices['Jadalnia']);
local temp_srednia_pietro = (temp_szymek + temp_marysia + temp_sypialnia + temp_gabinet) / 4; --array_temp_pietro.sum / #array_temp_pietro; --(temp_szymek + temp_marysia + temp_sypialnia + temp_gabinet) / 4;
local fire_power = otherdevices['Moc pracy palnika'];
local temp_refer_parter = tonumber(otherdevices_svalues['Temperatura zadana parter']);
local temp_refer_pietro = tonumber(otherdevices_svalues['Temperatura zadana piętro']);
local pompa_co = otherdevices['Pompa CO kocioł'];
local temp_flow_desired = tonumber(otherdevices_svalues['Temp. zasil. zadana']);
local temp_flow = tonumber(otherdevices_svalues['Temp. zasilania CO']);
local temp_ret = tonumber(otherdevices_svalues['Temp. powrotu CO']);

-- sprawdzenie czy czujnik w salonie jest aktualny i jeśli nie to czytamy z czujnika w jadalni
if timedifference(otherdevices_lastupdate['Salon']) > 900 then
    temp_salon = temp_jadalnia; --tonumber(otherdevices_svalues['Jadalnia']);
    MyLog('Nieaktualna Temperatura -> Czujnik w salonie nie jest aktualny! Oszacowano na podstawie czujnika w jadalni.');
end

-- sprawdzenie czy czujnik w sypialni jest aktualny i jeśli nie to czytamy z czujnika w gabinecie
if timedifference(otherdevices_lastupdate['Sypialnia']) > 900 then
    temp_sypialnia = temp_gabinet;
    MyLog('Nieaktualna Temperatura -> Czujnik w Sypialni nie jest aktualny! Oszacowano na podstawie czujnika w Gabinecie.');
end

-- sprawdzenie czy czujnik w pokoju marysi jest aktualny i jeśli nie to czytamy z czujnika z sypialni
if timedifference(otherdevices_lastupdate['Pokój Marysi']) > 900 then
    temp_marysia = temp_sypialnia + 0.5; -- dodanie 0.5 do temperatury w sypialni.
    MyLog('Nieaktualna Temperatura -> Czujnik w pokoju Marysi nie jest aktualny! Oszacowano na podstawie czujnika w Sypialni.');
end

-- sprawdzenie czy czujnik w pokoju szymka jest aktualny i jeśli nie to czytamy z czujnika w pokoju marysi
if timedifference(otherdevices_lastupdate['Pokój Szymka']) > 900 then
    temp_szymek = temp_marysia;
    MyLog('Nieaktualna Temperatura -> Czujnik w pokoju Szymka nie jest aktualny! Oszacowano na podstawie czujnika w pokoju Marysi.');
end

-- inteligentne sterowanie ogrzewaniem podłogowym. Szybkie podgrzewanie w przypadku dużej różnicy od zadanych temperatur.
if (ai_heating == 'On') then
    -- jeśli temp zewnętrzna wyższa niż
    if (temp_outside > temp_prog_lato) then
        floor_active = 'Off';
        MyLog('Włączono grzanie kaloryferami ze względu na temperaturę zewnętrzną > '..temp_prog_lato..' stopni.');
    -- grzanie kaloryferami z wysoką krzywą grzewczą w przypadku jak temperatura jest dużo niższa od zadanej
    elseif (temp_salon < temp_refer_parter - 0.2) or (temp_srednia_pietro < temp_refer_pietro - 0.2) then
        floor_active = 'Off'; 
        temp_day = temp_desired_floor;
        temp_night = temp_desired_floor;
        MyLog('Włączono grzanie kaloryferami ze względu na różnicę temperatur od zadanych');
    else
        floor_active = otherdevices['Podłogówka'];
        MyLog('Ogrzewanie inteligentne przełączone w tryb odmyślny.');
    end
    
end

-- sprawdzenie, czy podłogówka jest włączona
if (floor_active == 'On') then
    
    -- kontrola krzywej grzewczej w zależności od włączenia podłogówki
    if (HC_control == 'On') and (HC_current ~= HC_floor) then
        os.execute('ebusctl w -c 470 Hc1HeatCurve '..HC_floor);
        MyLog('Zmieniono krzywą grzewczą na profil podłogowy');
    end

    -- kontrola pompy parter ze względu na temperaturę w salonie
    if (temp_salon < temp_refer_parter) then
        pompa_parter = 'On';
        temp_day = temp_desired_floor;
        temp_night = temp_desired_floor;
    end
    
    -- kontrola pompy piętro ze względu na temeraturę w pokoju marysi lub szymka
    if (temp_srednia_pietro < temp_refer_pietro) then
        pompa_pietro = 'On';
        temp_day = temp_desired_floor;
        temp_night = temp_desired_floor;
    end
    
    -- kontrola pomp ze względu na pracę pompy w piecu
    if (pompa_co == 'Off') then
        pompa_parter = 'Off';
        pompa_pietro = 'Off';
        MyLog('Wyłączono pompy ze względu na wyłaczoną pompę CO w piecu.');
    end

    -- kontrola pomp ze względu na termperaturę wody zasilania lub pracy pompy w piecu
    if (temp_flow > 55) or (temp_flow < 25) or (temp_flow_desired > temp_flow + 7) then
        pompa_parter = 'Off';
        pompa_pietro = 'Off';
        MyLog('Wyłączono pompy ze względu na temperaturę zasilania CO.');
    end

else
    -- kontrola krzywej grzewczej w zależności od włączenia podłogówki
    if (HC_control == 'On') and (HC_current ~= HC_radiator) then
        os.execute('ebusctl w -c 470 Hc1HeatCurve '..HC_radiator);
        MyLog('Zmieniono krzywą grzewczą na profil kaloryferów');
    end
    
end

-- zmiana zadanej temperatury dziennej w sterowniku Vaillant
if otherdevices['Temp. dzienna'] ~= tostring(temp_day) then
    os.execute('ebusctl w -c 470 Hc1DayTemp '..temp_day);
end

-- zmiana zadanej temperatury nocnej w sterowniku Vaillant
if otherdevices['Temp. nocna'] ~= tostring(temp_night) then
    os.execute('ebusctl w -c 470 Hc1NightTemp '..temp_night);
end

-- przełączenie pompy CO parter
if otherdevices['Pompa CO parter'] ~= pompa_parter then
    commandArray['Pompa CO parter'] = pompa_parter;
end

-- przełączenie pompy CO piętro
if otherdevices['Pompa CO piętro'] ~= pompa_pietro then
    commandArray['Pompa CO piętro'] = pompa_pietro;
end

MyLog('=============================================');
MyLog('Ogrzewanie podłogowe = '..floor_active);
MyLog('Krzywa grzewcza = '..HC_current);
MyLog('Temperatura salon = '..temp_salon..'°C');
MyLog('Temperatura szymek = '..temp_szymek..'°C');
MyLog('Temperatura marysia = '..temp_marysia..'°C');
MyLog('Temperatura sypialnia = '..temp_sypialnia..'°C');
MyLog('Temperatura gabinet = '..temp_gabinet..'°C');
MyLog('Temperatura jadalnia = '..temp_jadalnia..'°C');
MyLog('Temperatura średnia piętro = '..temp_srednia_pietro..'°C');
MyLog('Pompa co = '..pompa_co);
MyLog('Pompa parter = '..pompa_parter);
MyLog('Pompa pietro = '..pompa_pietro);
MyLog('Temepratura zadana parter = '..temp_refer_parter..'°C');
MyLog('Temperatura zadana piętro = '..temp_refer_pietro..'°C');
MyLog('Tempertura zadana dzienna na sterowniku = '..temp_day..'°C');
MyLog('Tempertura zadana nocna na sterowniku = '..temp_night..'°C');
MyLog('Temperatura zasilania zadana = '..temp_flow_desired..'°C');
MyLog('Moc pracy palnika: '..fire_power..'%');
MyLog('Temperatura zasilania = '..temp_flow..'°C');
MyLog('Temperatura powrotu = '..temp_ret..'°C');
MyLog('Temperatura zewnętrzna = '..temp_outside..'°C');

return commandArray;
