using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using System.Drawing;
using IniParser;
using IniParser.Model;
using utility;
using System.Json;
using System.Text;
using System.Diagnostics;

//using Microsoft.VisualBasic.Strings;
namespace uclliu
{
    public class uclliu
    {
        myinclude my = new myinclude();
        public string VERSION = "0.1";
        public FileStream lockFileString;

        //把 chardefs 的字碼，變成對照字根，可以加速 ,,,z、,,,x 反查的速度
        public Dictionary<string, string> uclcode_r = new Dictionary<string, string>();
        public JsonValue uclcode = null;
        public bool is_DEBUG_mode = false; //除錯模式
        public string INI_CONFIG_FILE = "C:\\temp\\UCLLIU.ini"; //預設在 此，實際使用的位置同在 uclliu.exe
        string DEFAULT_OUTPUT_TYPE = "DEFAULT";
        //硬派出字方式選擇
        //#DEFAULT
        //#BIG5
        //#PASTE
        public List<string> sendkey_paste_ctrl_v_apps = new List<string>(); //使用複製文字貼上出字的 ctrl + v app
        public List<string> sendkey_paste_shift_ins_apps = new List<string>(); //使用複製文字貼上出字的 shift + ins app
        public List<string> sendkey_paste_big5_apps = new List<string>(); //使用 big5 複製文字貼上出字的 app
        public List<string> sendkey_not_use_ucl_apps = new List<string>(); //無法使用肥米的 app
        private string TC_CDATA = "万与丑专业丛东丝丢两严丧个丬丰临为丽举么义乌乐乔习乡书买乱争于亏云亘亚产亩亲亵亸亿仅从仑仓仪们价众优伙会伛伞伟传伤伥伦伧伪伫体余佣佥侠侣侥侦侧侨侩侪侬俣俦俨俩俪俭债倾偬偻偾偿傥傧储傩儿兑兖党兰关兴兹养兽冁内冈册写军农冢冯冲决况冻净凄凉凌减凑凛几凤凫凭凯击凼凿刍划刘则刚创删别刬刭刽刿剀剂剐剑剥剧劝办务劢动励劲劳势勋勐勚匀匦匮区医华协单卖卢卤卧卫却卺厂厅历厉压厌厍厕厢厣厦厨厩厮县参叆叇双发变叙叠叶号叹叽吁后吓吕吗吣吨听启吴呒呓呕呖呗员呙呛呜咏咔咙咛咝咤咴咸哌响哑哒哓哔哕哗哙哜哝哟唛唝唠唡唢唣唤唿啧啬啭啮啰啴啸喷喽喾嗫呵嗳嘘嘤嘱噜噼嚣嚯团园囱围囵国图圆圣圹场坂坏块坚坛坜坝坞坟坠垄垅垆垒垦垧垩垫垭垯垱垲垴埘埙埚埝埯堑堕塆墙壮声壳壶壸处备复够头夸夹夺奁奂奋奖奥妆妇妈妩妪妫姗姜娄娅娆娇娈娱娲娴婳婴婵婶媪嫒嫔嫱嬷孙学孪宁宝实宠审宪宫宽宾寝对寻导寿将尔尘尧尴尸尽层屃屉届属屡屦屿岁岂岖岗岘岙岚岛岭岳岽岿峃峄峡峣峤峥峦崂崃崄崭嵘嵚嵛嵝嵴巅巩巯币帅师帏帐帘帜带帧帮帱帻帼幂幞干并广庄庆庐庑库应庙庞废庼廪开异弃张弥弪弯弹强归当录彟彦彻径徕御忆忏忧忾怀态怂怃怄怅怆怜总怼怿恋恳恶恸恹恺恻恼恽悦悫悬悭悯惊惧惨惩惫惬惭惮惯愍愠愤愦愿慑慭憷懑懒懔戆戋戏戗战戬户扎扑扦执扩扪扫扬扰抚抛抟抠抡抢护报担拟拢拣拥拦拧拨择挂挚挛挜挝挞挟挠挡挢挣挤挥挦捞损捡换捣据捻掳掴掷掸掺掼揸揽揿搀搁搂搅携摄摅摆摇摈摊撄撑撵撷撸撺擞攒敌敛数斋斓斗斩断无旧时旷旸昙昼昽显晋晒晓晔晕晖暂暧札术朴机杀杂权条来杨杩杰极构枞枢枣枥枧枨枪枫枭柜柠柽栀栅标栈栉栊栋栌栎栏树栖样栾桊桠桡桢档桤桥桦桧桨桩梦梼梾检棂椁椟椠椤椭楼榄榇榈榉槚槛槟槠横樯樱橥橱橹橼檐檩欢欤欧歼殁殇残殒殓殚殡殴毁毂毕毙毡毵氇气氢氩氲汇汉污汤汹沓沟没沣沤沥沦沧沨沩沪沵泞泪泶泷泸泺泻泼泽泾洁洒洼浃浅浆浇浈浉浊测浍济浏浐浑浒浓浔浕涂涌涛涝涞涟涠涡涢涣涤润涧涨涩淀渊渌渍渎渐渑渔渖渗温游湾湿溃溅溆溇滗滚滞滟滠满滢滤滥滦滨滩滪漤潆潇潋潍潜潴澜濑濒灏灭灯灵灾灿炀炉炖炜炝点炼炽烁烂烃烛烟烦烧烨烩烫烬热焕焖焘煅煳熘爱爷牍牦牵牺犊犟状犷犸犹狈狍狝狞独狭狮狯狰狱狲猃猎猕猡猪猫猬献獭玑玙玚玛玮环现玱玺珉珏珐珑珰珲琎琏琐琼瑶瑷璇璎瓒瓮瓯电画畅畲畴疖疗疟疠疡疬疮疯疱疴痈痉痒痖痨痪痫痴瘅瘆瘗瘘瘪瘫瘾瘿癞癣癫癯皑皱皲盏盐监盖盗盘眍眦眬着睁睐睑瞒瞩矫矶矾矿砀码砖砗砚砜砺砻砾础硁硅硕硖硗硙硚确硷碍碛碜碱碹磙礼祎祢祯祷祸禀禄禅离秃秆种积称秽秾稆税稣稳穑穷窃窍窑窜窝窥窦窭竖竞笃笋笔笕笺笼笾筑筚筛筜筝筹签简箓箦箧箨箩箪箫篑篓篮篱簖籁籴类籼粜粝粤粪粮糁糇紧絷纟纠纡红纣纤纥约级纨纩纪纫纬纭纮纯纰纱纲纳纴纵纶纷纸纹纺纻纼纽纾线绀绁绂练组绅细织终绉绊绋绌绍绎经绐绑绒结绔绕绖绗绘给绚绛络绝绞统绠绡绢绣绤绥绦继绨绩绪绫绬续绮绯绰绱绲绳维绵绶绷绸绹绺绻综绽绾绿缀缁缂缃缄缅缆缇缈缉缊缋缌缍缎缏缐缑缒缓缔缕编缗缘缙缚缛缜缝缞缟缠缡缢缣缤缥缦缧缨缩缪缫缬缭缮缯缰缱缲缳缴缵罂网罗罚罢罴羁羟羡翘翙翚耢耧耸耻聂聋职聍联聩聪肃肠肤肷肾肿胀胁胆胜胧胨胪胫胶脉脍脏脐脑脓脔脚脱脶脸腊腌腘腭腻腼腽腾膑臜舆舣舰舱舻艰艳艹艺节芈芗芜芦苁苇苈苋苌苍苎苏苘苹茎茏茑茔茕茧荆荐荙荚荛荜荞荟荠荡荣荤荥荦荧荨荩荪荫荬荭荮药莅莜莱莲莳莴莶获莸莹莺莼萚萝萤营萦萧萨葱蒇蒉蒋蒌蓝蓟蓠蓣蓥蓦蔷蔹蔺蔼蕲蕴薮藁藓虏虑虚虫虬虮虽虾虿蚀蚁蚂蚕蚝蚬蛊蛎蛏蛮蛰蛱蛲蛳蛴蜕蜗蜡蝇蝈蝉蝎蝼蝾螀螨蟏衅衔补衬衮袄袅袆袜袭袯装裆裈裢裣裤裥褛褴襁襕见观觃规觅视觇览觉觊觋觌觍觎觏觐觑觞触觯詟誉誊讠计订讣认讥讦讧讨让讪讫训议讯记讱讲讳讴讵讶讷许讹论讻讼讽设访诀证诂诃评诅识诇诈诉诊诋诌词诎诏诐译诒诓诔试诖诗诘诙诚诛诜话诞诟诠诡询诣诤该详诧诨诩诪诫诬语诮误诰诱诲诳说诵诶请诸诹诺读诼诽课诿谀谁谂调谄谅谆谇谈谊谋谌谍谎谏谐谑谒谓谔谕谖谗谘谙谚谛谜谝谞谟谠谡谢谣谤谥谦谧谨谩谪谫谬谭谮谯谰谱谲谳谴谵谶谷豮贝贞负贠贡财责贤败账货质贩贪贫贬购贮贯贰贱贲贳贴贵贶贷贸费贺贻贼贽贾贿赀赁赂赃资赅赆赇赈赉赊赋赌赍赎赏赐赑赒赓赔赕赖赗赘赙赚赛赜赝赞赟赠赡赢赣赪赵赶趋趱趸跃跄跖跞践跶跷跸跹跻踊踌踪踬踯蹑蹒蹰蹿躏躜躯车轧轨轩轪轫转轭轮软轰轱轲轳轴轵轶轷轸轹轺轻轼载轾轿辀辁辂较辄辅辆辇辈辉辊辋辌辍辎辏辐辑辒输辔辕辖辗辘辙辚辞辩辫边辽达迁过迈运还这进远违连迟迩迳迹适选逊递逦逻遗遥邓邝邬邮邹邺邻郁郄郏郐郑郓郦郧郸酝酦酱酽酾酿释里鉅鉴銮錾钆钇针钉钊钋钌钍钎钏钐钑钒钓钔钕钖钗钘钙钚钛钝钞钟钠钡钢钣钤钥钦钧钨钩钪钫钬钭钮钯钰钱钲钳钴钵钶钷钸钹钺钻钼钽钾钿铀铁铂铃铄铅铆铈铉铊铋铍铎铏铐铑铒铕铗铘铙铚铛铜铝铞铟铠铡铢铣铤铥铦铧铨铪铫铬铭铮铯铰铱铲铳铴铵银铷铸铹铺铻铼铽链铿销锁锂锃锄锅锆锇锈锉锊锋锌锍锎锏锐锑锒锓锔锕锖锗错锚锜锞锟锠锡锢锣锤锥锦锨锩锫锬锭键锯锰锱锲锳锴锵锶锷锸锹锺锻锼锽锾锿镀镁镂镃镆镇镈镉镊镌镍镎镏镐镑镒镕镖镗镙镚镛镜镝镞镟镠镡镢镣镤镥镦镧镨镩镪镫镬镭镮镯镰镱镲镳镴镶长门闩闪闫闬闭问闯闰闱闲闳间闵闶闷闸闹闺闻闼闽闾闿阀阁阂阃阄阅阆阇阈阉阊阋阌阍阎阏阐阑阒阓阔阕阖阗阘阙阚阛队阳阴阵阶际陆陇陈陉陕陧陨险随隐隶隽难雏雠雳雾霁霉霭靓静靥鞑鞒鞯鞴韦韧韨韩韪韫韬韵页顶顷顸项顺须顼顽顾顿颀颁颂颃预颅领颇颈颉颊颋颌颍颎颏颐频颒颓颔颕颖颗题颙颚颛颜额颞颟颠颡颢颣颤颥颦颧风飏飐飑飒飓飔飕飖飗飘飙飚飞飨餍饤饥饦饧饨饩饪饫饬饭饮饯饰饱饲饳饴饵饶饷饸饹饺饻饼饽饾饿馀馁馂馃馄馅馆馇馈馉馊馋馌馍馎馏馐馑馒馓馔馕马驭驮驯驰驱驲驳驴驵驶驷驸驹驺驻驼驽驾驿骀骁骂骃骄骅骆骇骈骉骊骋验骍骎骏骐骑骒骓骔骕骖骗骘骙骚骛骜骝骞骟骠骡骢骣骤骥骦骧髅髋髌鬓魇魉鱼鱽鱾鱿鲀鲁鲂鲄鲅鲆鲇鲈鲉鲊鲋鲌鲍鲎鲏鲐鲑鲒鲓鲔鲕鲖鲗鲘鲙鲚鲛鲜鲝鲞鲟鲠鲡鲢鲣鲤鲥鲦鲧鲨鲩鲪鲫鲬鲭鲮鲯鲰鲱鲲鲳鲴鲵鲶鲷鲸鲹鲺鲻鲼鲽鲾鲿鳀鳁鳂鳃鳄鳅鳆鳇鳈鳉鳊鳋鳌鳍鳎鳏鳐鳑鳒鳓鳔鳕鳖鳗鳘鳙鳛鳜鳝鳞鳟鳠鳡鳢鳣鸟鸠鸡鸢鸣鸤鸥鸦鸧鸨鸩鸪鸫鸬鸭鸮鸯鸰鸱鸲鸳鸴鸵鸶鸷鸸鸹鸺鸻鸼鸽鸾鸿鹀鹁鹂鹃鹄鹅鹆鹇鹈鹉鹊鹋鹌鹍鹎鹏鹐鹑鹒鹓鹔鹕鹖鹗鹘鹚鹛鹜鹝鹞鹟鹠鹡鹢鹣鹤鹥鹦鹧鹨鹩鹪鹫鹬鹭鹯鹰鹱鹲鹳鹴鹾麦麸黄黉黡黩黪黾鼋鼌鼍鼗鼹齄齐齑齿龀龁龂龃龄龅龆龇龈龉龊龋龌龙龚龛龟志制咨只里系范松没尝尝闹面准钟别闲干尽脏拼";
        private string TC_TDATA = "萬與醜專業叢東絲丟兩嚴喪個爿豐臨為麗舉麼義烏樂喬習鄉書買亂爭於虧雲亙亞產畝親褻嚲億僅從侖倉儀們價眾優夥會傴傘偉傳傷倀倫傖偽佇體餘傭僉俠侶僥偵側僑儈儕儂俁儔儼倆儷儉債傾傯僂僨償儻儐儲儺兒兌兗黨蘭關興茲養獸囅內岡冊寫軍農塚馮衝決況凍淨淒涼淩減湊凜幾鳳鳧憑凱擊氹鑿芻劃劉則剛創刪別剗剄劊劌剴劑剮劍剝劇勸辦務勱動勵勁勞勢勳猛勩勻匭匱區醫華協單賣盧鹵臥衛卻巹廠廳曆厲壓厭厙廁廂厴廈廚廄廝縣參靉靆雙發變敘疊葉號歎嘰籲後嚇呂嗎唚噸聽啟吳嘸囈嘔嚦唄員咼嗆嗚詠哢嚨嚀噝吒噅鹹呱響啞噠嘵嗶噦嘩噲嚌噥喲嘜嗊嘮啢嗩唕喚呼嘖嗇囀齧囉嘽嘯噴嘍嚳囁嗬噯噓嚶囑嚕劈囂謔團園囪圍圇國圖圓聖壙場阪壞塊堅壇壢壩塢墳墜壟壟壚壘墾坰堊墊埡墶壋塏堖塒塤堝墊垵塹墮壪牆壯聲殼壺壼處備複夠頭誇夾奪奩奐奮獎奧妝婦媽嫵嫗媯姍薑婁婭嬈嬌孌娛媧嫻嫿嬰嬋嬸媼嬡嬪嬙嬤孫學孿寧寶實寵審憲宮寬賓寢對尋導壽將爾塵堯尷屍盡層屭屜屆屬屢屨嶼歲豈嶇崗峴嶴嵐島嶺嶽崠巋嶨嶧峽嶢嶠崢巒嶗崍嶮嶄嶸嶔崳嶁脊巔鞏巰幣帥師幃帳簾幟帶幀幫幬幘幗冪襆幹並廣莊慶廬廡庫應廟龐廢廎廩開異棄張彌弳彎彈強歸當錄彠彥徹徑徠禦憶懺憂愾懷態慫憮慪悵愴憐總懟懌戀懇惡慟懨愷惻惱惲悅愨懸慳憫驚懼慘懲憊愜慚憚慣湣慍憤憒願懾憖怵懣懶懍戇戔戲戧戰戩戶紮撲扡執擴捫掃揚擾撫拋摶摳掄搶護報擔擬攏揀擁攔擰撥擇掛摯攣掗撾撻挾撓擋撟掙擠揮撏撈損撿換搗據撚擄摑擲撣摻摜摣攬撳攙擱摟攪攜攝攄擺搖擯攤攖撐攆擷擼攛擻攢敵斂數齋斕鬥斬斷無舊時曠暘曇晝曨顯晉曬曉曄暈暉暫曖劄術樸機殺雜權條來楊榪傑極構樅樞棗櫪梘棖槍楓梟櫃檸檉梔柵標棧櫛櫳棟櫨櫟欄樹棲樣欒棬椏橈楨檔榿橋樺檜槳樁夢檮棶檢欞槨櫝槧欏橢樓欖櫬櫚櫸檟檻檳櫧橫檣櫻櫫櫥櫓櫞簷檁歡歟歐殲歿殤殘殞殮殫殯毆毀轂畢斃氈毿氌氣氫氬氳匯漢汙湯洶遝溝沒灃漚瀝淪滄渢溈滬濔濘淚澩瀧瀘濼瀉潑澤涇潔灑窪浹淺漿澆湞溮濁測澮濟瀏滻渾滸濃潯濜塗湧濤澇淶漣潿渦溳渙滌潤澗漲澀澱淵淥漬瀆漸澠漁瀋滲溫遊灣濕潰濺漵漊潷滾滯灩灄滿瀅濾濫灤濱灘澦濫瀠瀟瀲濰潛瀦瀾瀨瀕灝滅燈靈災燦煬爐燉煒熗點煉熾爍爛烴燭煙煩燒燁燴燙燼熱煥燜燾煆糊溜愛爺牘犛牽犧犢強狀獷獁猶狽麅獮獰獨狹獅獪猙獄猻獫獵獼玀豬貓蝟獻獺璣璵瑒瑪瑋環現瑲璽瑉玨琺瓏璫琿璡璉瑣瓊瑤璦璿瓔瓚甕甌電畫暢佘疇癤療瘧癘瘍鬁瘡瘋皰屙癰痙癢瘂癆瘓癇癡癉瘮瘞瘺癟癱癮癭癩癬癲臒皚皺皸盞鹽監蓋盜盤瞘眥矓著睜睞瞼瞞矚矯磯礬礦碭碼磚硨硯碸礪礱礫礎硜矽碩硤磽磑礄確鹼礙磧磣堿镟滾禮禕禰禎禱禍稟祿禪離禿稈種積稱穢穠穭稅穌穩穡窮竊竅窯竄窩窺竇窶豎競篤筍筆筧箋籠籩築篳篩簹箏籌簽簡籙簀篋籜籮簞簫簣簍籃籬籪籟糴類秈糶糲粵糞糧糝餱緊縶糸糾紆紅紂纖紇約級紈纊紀紉緯紜紘純紕紗綱納紝縱綸紛紙紋紡紵紖紐紓線紺絏紱練組紳細織終縐絆紼絀紹繹經紿綁絨結絝繞絰絎繪給絢絳絡絕絞統綆綃絹繡綌綏絛繼綈績緒綾緓續綺緋綽緔緄繩維綿綬繃綢綯綹綣綜綻綰綠綴緇緙緗緘緬纜緹緲緝縕繢緦綞緞緶線緱縋緩締縷編緡緣縉縛縟縝縫縗縞纏縭縊縑繽縹縵縲纓縮繆繅纈繚繕繒韁繾繰繯繳纘罌網羅罰罷羆羈羥羨翹翽翬耮耬聳恥聶聾職聹聯聵聰肅腸膚膁腎腫脹脅膽勝朧腖臚脛膠脈膾髒臍腦膿臠腳脫腡臉臘醃膕齶膩靦膃騰臏臢輿艤艦艙艫艱豔艸藝節羋薌蕪蘆蓯葦藶莧萇蒼苧蘇檾蘋莖蘢蔦塋煢繭荊薦薘莢蕘蓽蕎薈薺蕩榮葷滎犖熒蕁藎蓀蔭蕒葒葤藥蒞蓧萊蓮蒔萵薟獲蕕瑩鶯蓴蘀蘿螢營縈蕭薩蔥蕆蕢蔣蔞藍薊蘺蕷鎣驀薔蘞藺藹蘄蘊藪槁蘚虜慮虛蟲虯蟣雖蝦蠆蝕蟻螞蠶蠔蜆蠱蠣蟶蠻蟄蛺蟯螄蠐蛻蝸蠟蠅蟈蟬蠍螻蠑螿蟎蠨釁銜補襯袞襖嫋褘襪襲襏裝襠褌褳襝褲襇褸襤繈襴見觀覎規覓視覘覽覺覬覡覿覥覦覯覲覷觴觸觶讋譽謄訁計訂訃認譏訐訌討讓訕訖訓議訊記訒講諱謳詎訝訥許訛論訩訟諷設訪訣證詁訶評詛識詗詐訴診詆謅詞詘詔詖譯詒誆誄試詿詩詰詼誠誅詵話誕詬詮詭詢詣諍該詳詫諢詡譸誡誣語誚誤誥誘誨誑說誦誒請諸諏諾讀諑誹課諉諛誰諗調諂諒諄誶談誼謀諶諜謊諫諧謔謁謂諤諭諼讒諮諳諺諦謎諞諝謨讜謖謝謠謗諡謙謐謹謾謫譾謬譚譖譙讕譜譎讞譴譫讖穀豶貝貞負貟貢財責賢敗賬貨質販貪貧貶購貯貫貳賤賁貰貼貴貺貸貿費賀貽賊贄賈賄貲賃賂贓資賅贐賕賑賚賒賦賭齎贖賞賜贔賙賡賠賧賴賵贅賻賺賽賾贗讚贇贈贍贏贛赬趙趕趨趲躉躍蹌蹠躒踐躂蹺蹕躚躋踴躊蹤躓躑躡蹣躕躥躪躦軀車軋軌軒軑軔轉軛輪軟轟軲軻轤軸軹軼軤軫轢軺輕軾載輊轎輈輇輅較輒輔輛輦輩輝輥輞輬輟輜輳輻輯轀輸轡轅轄輾轆轍轔辭辯辮邊遼達遷過邁運還這進遠違連遲邇逕跡適選遜遞邐邏遺遙鄧鄺鄔郵鄒鄴鄰鬱郤郟鄶鄭鄆酈鄖鄲醞醱醬釅釃釀釋裏钜鑒鑾鏨釓釔針釘釗釙釕釷釺釧釤鈒釩釣鍆釹鍚釵鈃鈣鈈鈦鈍鈔鍾鈉鋇鋼鈑鈐鑰欽鈞鎢鉤鈧鈁鈥鈄鈕鈀鈺錢鉦鉗鈷缽鈳鉕鈽鈸鉞鑽鉬鉭鉀鈿鈾鐵鉑鈴鑠鉛鉚鈰鉉鉈鉍鈹鐸鉶銬銠鉺銪鋏鋣鐃銍鐺銅鋁銱銦鎧鍘銖銑鋌銩銛鏵銓鉿銚鉻銘錚銫鉸銥鏟銃鐋銨銀銣鑄鐒鋪鋙錸鋱鏈鏗銷鎖鋰鋥鋤鍋鋯鋨鏽銼鋝鋒鋅鋶鐦鐧銳銻鋃鋟鋦錒錆鍺錯錨錡錁錕錩錫錮鑼錘錐錦鍁錈錇錟錠鍵鋸錳錙鍥鍈鍇鏘鍶鍔鍤鍬鍾鍛鎪鍠鍰鎄鍍鎂鏤鎡鏌鎮鎛鎘鑷鐫鎳鎿鎦鎬鎊鎰鎔鏢鏜鏍鏰鏞鏡鏑鏃鏇鏐鐔钁鐐鏷鑥鐓鑭鐠鑹鏹鐙鑊鐳鐶鐲鐮鐿鑔鑣鑞鑲長門閂閃閆閈閉問闖閏闈閑閎間閔閌悶閘鬧閨聞闥閩閭闓閥閣閡閫鬮閱閬闍閾閹閶鬩閿閽閻閼闡闌闃闠闊闋闔闐闒闕闞闤隊陽陰陣階際陸隴陳陘陝隉隕險隨隱隸雋難雛讎靂霧霽黴靄靚靜靨韃鞽韉韝韋韌韍韓韙韞韜韻頁頂頃頇項順須頊頑顧頓頎頒頌頏預顱領頗頸頡頰頲頜潁熲頦頤頻頮頹頷頴穎顆題顒顎顓顏額顳顢顛顙顥纇顫顬顰顴風颺颭颮颯颶颸颼颻飀飄飆飆飛饗饜飣饑飥餳飩餼飪飫飭飯飲餞飾飽飼飿飴餌饒餉餄餎餃餏餅餑餖餓餘餒餕餜餛餡館餷饋餶餿饞饁饃餺餾饈饉饅饊饌饢馬馭馱馴馳驅馹駁驢駔駛駟駙駒騶駐駝駑駕驛駘驍罵駰驕驊駱駭駢驫驪騁驗騂駸駿騏騎騍騅騌驌驂騙騭騤騷騖驁騮騫騸驃騾驄驏驟驥驦驤髏髖髕鬢魘魎魚魛魢魷魨魯魴魺鮁鮃鯰鱸鮋鮓鮒鮊鮑鱟鮍鮐鮭鮚鮳鮪鮞鮦鰂鮜鱠鱭鮫鮮鮺鯗鱘鯁鱺鰱鰹鯉鰣鰷鯀鯊鯇鮶鯽鯒鯖鯪鯕鯫鯡鯤鯧鯝鯢鯰鯛鯨鯵鯴鯔鱝鰈鰏鱨鯷鰮鰃鰓鱷鰍鰒鰉鰁鱂鯿鰠鼇鰭鰨鰥鰩鰟鰜鰳鰾鱈鱉鰻鰵鱅鰼鱖鱔鱗鱒鱯鱤鱧鱣鳥鳩雞鳶鳴鳲鷗鴉鶬鴇鴆鴣鶇鸕鴨鴞鴦鴒鴟鴝鴛鴬鴕鷥鷙鴯鴰鵂鴴鵃鴿鸞鴻鵐鵓鸝鵑鵠鵝鵒鷳鵜鵡鵲鶓鵪鶤鵯鵬鵮鶉鶊鵷鷫鶘鶡鶚鶻鶿鶥鶩鷊鷂鶲鶹鶺鷁鶼鶴鷖鸚鷓鷚鷯鷦鷲鷸鷺鸇鷹鸌鸏鸛鸘鹺麥麩黃黌黶黷黲黽黿鼂鼉鞀鼴齇齊齏齒齔齕齗齟齡齙齠齜齦齬齪齲齷龍龔龕龜誌製谘隻裡係範鬆冇嚐嘗鬨麵準鐘彆閒乾儘臟拚";
        public List<string> m_TC_CDATA = new List<string>();
        public IniData config = new IniData();
        public string last_key = ""; //用來紀錄最字的字碼，處理 ,,, 使用的
        public bool is_send_ucl = false;
        public bool flag_is_ucl = true;
        public bool flag_is_hf = true;
        public bool flag_is_play_otherkey = false;
        public bool flag_is_shift_down = false;
        public bool flag_is_gamemode = false;
        public string play_ucl_label = "";
        public string last_word_label_txt = "";
        public bool flag_is_win_down = false;
        public bool flag_is_capslock_down = false;
        public bool flag_is_play_capslock_otherkey = false;
        public bool flag_is_ctrl_down = false;
        public string same_sound_last_word = "";
        public bool is_need_use_pinyi = false;
        public List<string> ucl_find_data = new List<string>();
        int same_sound_index = 0; //用來放第幾頁
        int same_sound_max_word = 6; //一頁最多5字
        bool is_has_more_page = false; //是否還有下頁
        bool is_display_sp = false; //是否顯示簡根
        //# GUI Font
        public Font GUI_FONT_12 = new Font("roman", 12, FontStyle.Bold);
        public Font GUI_FONT_14 = new Font("roman", 14, FontStyle.Bold);
        public Font GUI_FONT_16 = new Font("roman", 16, FontStyle.Bold);
        public Font GUI_FONT_18 = new Font("roman", 18, FontStyle.Bold);
        public Font GUI_FONT_20 = new Font("roman", 20, FontStyle.Bold);
        public Font GUI_FONT_22 = new Font("roman", 22, FontStyle.Bold);
        public Font GUI_FONT_26 = new Font("roman", 26, FontStyle.Bold);
        static Form1 f;
        public uclliu(ref Form1 _f)
        {
            f = _f;


            string apps = "";
            //使用複製文字貼上出字的 app (ctrl+v)
            apps = "oxygennotincluded.exe,iedit_.exe";
            var m = my.explode(",", apps.ToLower());
            for (int i = 0, max_i = m.Length; i < max_i; i++)
            {
                m[i] = my.mainname(m[i]);
            }
            sendkey_paste_ctrl_v_apps = new List<string>(m);

            //使用複製文字貼上出字的 app (shift+ins)
            apps = "putty,pietty,pcman,xyplorer,kinza.exe,iedit.exe";
            m = my.explode(",", apps.ToLower());
            for (int i = 0, max_i = m.Length; i < max_i; i++)
            {
                m[i] = my.mainname(m[i]);
            }
            sendkey_paste_shift_ins_apps = new List<string>(m);

            //使用 big5 複製文字貼上出字的 app
            apps = "zip32w,daqkingcon.exe,EWinner.exe";
            m = my.explode(",", apps.ToLower());
            for (int i = 0, max_i = m.Length; i < max_i; i++)
            {
                m[i] = my.mainname(m[i]);
            }
            sendkey_paste_big5_apps = new List<string>(m);

            //無法使用肥米的 app
            apps = "mstsc.exe";
            m = my.explode(",", apps.ToLower());
            for (int i = 0, max_i = m.Length; i < max_i; i++)
            {
                m[i] = my.mainname(m[i]);
            }
            sendkey_not_use_ucl_apps = new List<string>(m);

            //f.Enabled = true;
            //f.btn_UCL.Text = "GG";
        }
        public string simple2trad(string data)
        {
            //殘轉正
            var m = new List<string>();
            for (int i = 0, max_i = data.Length; i < max_i; i++)
            {
                int is_find = TC_CDATA.IndexOf(data[i]);
                if (is_find == -1)
                {
                    m.Add(data[i].ToString());
                }
                else
                {
                    m.Add(TC_TDATA[is_find].ToString());
                }
            }
            return my.implode("", m);
        }
        public string trad2simple(string data)
        {
            //正轉殘
            var m = new List<string>();
            for (int i = 0, max_i = data.Length; i < max_i; i++)
            {                
                int is_find = TC_TDATA.IndexOf(data[i]);
                //Console.WriteLine("data[i]:" + data[i]);
                //Console.WriteLine("is_find:" + is_find.ToString());
                if (is_find == -1)
                {
                    m.Add(data[i].ToString());
                }
                else
                {
                    m.Add(TC_CDATA[is_find].ToString());
                }
            }
            return my.implode("", m);
        }
        public bool run_extra() //跑額外的功能，如 ,,,version
        {
            string code = "";
            //Console.WriteLine("last_key:" + last_key);
            code = ",,,c";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                //啟動殘體
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                toAlphaOrNonAlpha();
                if (is_ucl() == false)
                {
                    //# change to ucl
                    toggle_ucl();
                }
                f.btn_simple.Visible = true;
                update_UI();
            }
            code = ",,,t";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                //啟動正體
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                toAlphaOrNonAlpha();
                if (is_ucl() == false)
                {
                    //# change to ucl
                    toggle_ucl();
                }
                f.btn_simple.Visible = false;
                update_UI();
            }
            code = ",,,version";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                // https://stackoverflow.com/questions/16105097/why-isnt-messagebox-topmost
                // messagebox top most 的問題
                MessageBox.Show(new Form { TopMost = true },
                    about_uclliu(),
                    "羽山の說明",
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
                return true;
            }
            code = ",,,lock";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                last_key = "";
                if (!flag_is_gamemode)
                {
                    toggle_gamemode();
                }
                return true;
            }
            code = ",,,unlock";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                last_key = "";
                if (flag_is_gamemode)
                {
                    toggle_gamemode();
                }
                return true;
            }
            code = ",,,s";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                run_short();
                toAlphaOrNonAlpha();
                return true;
            }
            code = ",,,l";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                run_long();
                toAlphaOrNonAlpha();
                return true;
            }
            code = ",,,-";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                //# run small
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                toAlphaOrNonAlpha();
                run_big_small(-0.2);
                return true;
            }
            code = ",,,+";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                //# run big
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                toAlphaOrNonAlpha();
                run_big_small(0.2);
                return true;
            }
            return false;
        }
        public void run_big_small(double kind)
        {
            double z = Convert.ToDouble(config["DEFAULT"]["ZOOM"]);
            if (kind > 0)
            {
                if (z < 3)
                {
                    config["DEFAULT"]["ZOOM"] = (z + kind).ToString();
                }
            }
            else
            {
                if (z > 0.3)
                {
                    config["DEFAULT"]["ZOOM"] = (z + kind).ToString();
                }
            }
            update_UI();
            saveConfig();
        }
        public void run_short()
        {
            //f.word_label.Visible = false;
            //f.type_label.Vset_visible(False)
            f.btn_gamemode.Visible = false;
            config["DEFAULT"]["SHORT_MODE"] = "1";
            update_UI();
            saveConfig();
        }
        public void run_long()
        {
            //f.word_label.Visible = false;
            //f.type_label.Vset_visible(False)
            f.btn_gamemode.Visible = true;
            config["DEFAULT"]["SHORT_MODE"] = "0";
            update_UI();
            saveConfig();
        }
        public string about_uclliu()
        {
            string _msg_text = string.Format("肥米輸入法 C# 版\n\n作者：羽山秋人 (http://3wa.tw)\n版本：{0}", VERSION);
            _msg_text += "\n\n熱鍵提示：\n\n";
            _msg_text += "「,,,VERSION」目前版本\n";
            _msg_text += "「,,,UNLOCK」回到正常模式\n";
            _msg_text += "「,,,LOCK」進入遊戲模式\n";
            _msg_text += "「,,,C」簡體模式\n";
            _msg_text += "「,,,T」正體模式\n";
            _msg_text += "「,,,S」UI變窄\n";
            _msg_text += "「,,,L」UI變寬\n";
            _msg_text += "「,,,+」UI變大\n";
            _msg_text += "「,,,-」UI變小\n";
            _msg_text += "「,,,X」框字的字根轉回文字\n";
            _msg_text += "「,,,Z」框字的文字變成字根\n";
            return _msg_text;
        }
        ///字串轉全形
        /// From : https://dotblogs.com.tw/shunnien/2013/07/21/111737
        ///</summary>
        ///<param name="input">任一字元串</param>
        ///<returns>全形字元串</returns>
        private string ToWide(string input)
        {
            //半形轉全形：
            /*char[] c = input.ToCharArray();
            for (int i = 0; i < c.Length; i++)
            {
                //全形空格為12288，半形空格為32
                if (c[i] == 32)
                {
                    c[i] = (char)12288;
                    continue;
                }
                //其他字元半形(33-126)與全形(65281-65374)的對應關係是：均相差65248
                if (c[i] < 127)
                    c[i] = (char)(c[i] + 65248);
            }
            return new string(c);
            */
            //改用黑暗執行序的方法：https://blog.darkthread.net/blog/strconv-half-full-width-notes/
            //debug_print(input);
            //return Microsoft.VisualBasic.Strings.StrConv(input, Microsoft.VisualBasic.VbStrConv.Wide, 1028);
            return Microsoft.VisualBasic.Strings.StrConv(input, Microsoft.VisualBasic.VbStrConv.Wide, 1028);
        }
        public bool checkLockSuccess()
        {
            //開啟程式使用，檢查有沒有重複執行肥米
            //From : https://stackoverflow.com/questions/5522232/how-to-lock-a-file-with-c
            string check_file = my.pwd() + "\\UCLLIU.lock";
            if (!my.is_file(check_file))
            {
                my.file_put_contents(check_file, "");
            }
            try
            {
                debug_print("Lock file:" + check_file);
                lockFileString = new FileStream(check_file, FileMode.Open, FileAccess.Read, FileShare.None);
                return true;
            }
            catch (Exception ex)
            {
                debug_print("肥米已經被開啟了，取消..."+ex.Message);
                return false;
            }
        }
        public void loadJsonData()
        {
            string liu_json_path = my.pwd() + "\\liu.json";
            if (!my.is_file(liu_json_path))
            {
                MessageBox.Show("查無 liu.json 檔...");
                f.btn_X.PerformClick();
            }
            //uclcode = my.json_decode(my.file_get_contents(PWD + "\\liu.json"))            
            try
            {
                //JsonValue 使用 System.Json
                //https://stackoverflow.com/questions/6620165/how-can-i-parse-json-with-c
                string data = my.b2s(my.file_get_contents(liu_json_path));
                uclcode = JsonValue.Parse(data);
                debug_print(uclcode["chardefs"]["ucl"].ToString());
            }
            catch (Exception ex)
            {
                MessageBox.Show("liu.json 檔內容解算錯誤...");
                debug_print("liu.json 檔內容解算錯誤..." + ex.Message);
                f.btn_X.PerformClick();
            }
            //debug_print(uclcode["chardefs"]["addr"].ToString());

        }
        public void saveConfig()
        {
            FileIniDataParser _p = new FileIniDataParser();
            _p.WriteFile(INI_CONFIG_FILE, config);
        }
        public void loadConfig()
        {
            //#2019-03-02 調整，將 UCLLIU.ini 跟隨在 UCLLIU.exe 旁            
            if (my.is_file(INI_CONFIG_FILE))
            {
                my.copy(INI_CONFIG_FILE, my.pwd() + "\\UCLLIU.ini");
                my.unlink(INI_CONFIG_FILE);
            }
            INI_CONFIG_FILE = my.pwd() + "\\UCLLIU.ini";
            //load ini to db
            //https://github.com/rickyah/ini-parser
            var parser = new FileIniDataParser();

            int screen_width = Screen.PrimaryScreen.Bounds.Width;
            int screen_height = Screen.PrimaryScreen.Bounds.Height;
            config["DEFAULT"]["X"] = (screen_width - 700).ToString();
            config["DEFAULT"]["Y"] = (screen_height * 0.87).ToString();
            config["DEFAULT"]["ALPHA"] = "1"; //#嘸蝦米全顯示時時的初值
            config["DEFAULT"]["SHORT_MODE"] = "0"; // #0:簡短畫面，或1:長畫面
            config["DEFAULT"]["ZOOM"] = "1"; //#整體比例大小
            config["DEFAULT"]["SEND_KIND_1_PASTE"] = ""; //#出字模式1
            config["DEFAULT"]["SEND_KIND_2_BIG5"] = ""; //#出字模式2
            debug_print(INI_CONFIG_FILE);
            if (my.is_file(INI_CONFIG_FILE))
            {
                //轉回全大寫放回
                string data = my.b2s(my.file_get_contents(INI_CONFIG_FILE));
                data = data.ToUpper();
                my.file_put_contents(INI_CONFIG_FILE, data);
                try
                {
                    FileIniDataParser _p = new FileIniDataParser();
                    IniData _config = _p.ReadFile(INI_CONFIG_FILE);

                    foreach (var key in _config.Sections.GetSectionData("DEFAULT").Keys)
                    {
                        config["DEFAULT"][key.KeyName] = key.Value.Trim();
                        //debug_print(key.KeyName);
                        //debug_print(key.Value);
                    }
                }
                catch (Exception ex)
                {
                    debug_print("Config 可能有問題... " + INI_CONFIG_FILE);
                    debug_print(ex.Message);
                }
            }
            debug_print(config.ToString());
            if (Convert.ToDouble(config["DEFAULT"]["ALPHA"]) >= 1)
            {
                config["DEFAULT"]["ALPHA"] = "1";
            }
            if (Convert.ToDouble(config["DEFAULT"]["ALPHA"]) <= 0.1)
            {
                config["DEFAULT"]["ALPHA"] = "0.1";
            }
            if (Convert.ToInt32(config["DEFAULT"]["SHORT_MODE"]) >= 1)
            {
                config["DEFAULT"]["SHORT_MODE"] = "1";
            }
            if (Convert.ToInt32(config["DEFAULT"]["SHORT_MODE"]) <= 0)
            {
                config["DEFAULT"]["SHORT_MODE"] = "0";
            }
            if (Convert.ToDouble(config["DEFAULT"]["ZOOM"]) >= 3)
            {
                config["DEFAULT"]["ZOOM"] = "3";
            }
            if (Convert.ToDouble(config["DEFAULT"]["ZOOM"]) <= 0.1)
            {
                config["DEFAULT"]["ZOOM"] = "0.1";
            }
            update_UI();
            //不管如何，先存一次
            saveConfig();
        }
        public bool is_simple()
        {
            return f.btn_simple.Visible;
        }
        public void update_UI()
        {
            //修正畫面放大、縮小，調整大小， UI 變化很慢的問題
            //From : http://www.vbforums.com/showthread.php?632550-RESOLVED-Slow-Form-Load-
            f.SuspendLayout();
            //# GUI Font
            debug_print("font size : " + config["DEFAULT"]["ZOOM"]);
            GUI_FONT_12 = new Font("roman", Convert.ToInt32(Convert.ToDouble(config["DEFAULT"]["ZOOM"]) * 12), FontStyle.Bold);
            GUI_FONT_14 = new Font("roman", Convert.ToInt32(Convert.ToDouble(config["DEFAULT"]["ZOOM"]) * 14), FontStyle.Bold);
            GUI_FONT_16 = new Font("roman", Convert.ToInt32(Convert.ToDouble(config["DEFAULT"]["ZOOM"]) * 16), FontStyle.Bold);
            GUI_FONT_18 = new Font("roman", Convert.ToInt32(Convert.ToDouble(config["DEFAULT"]["ZOOM"]) * 18), FontStyle.Bold);
            GUI_FONT_20 = new Font("roman", Convert.ToInt32(Convert.ToDouble(config["DEFAULT"]["ZOOM"]) * 20), FontStyle.Bold);
            GUI_FONT_22 = new Font("roman", Convert.ToInt32(Convert.ToDouble(config["DEFAULT"]["ZOOM"]) * 22), FontStyle.Bold);
            GUI_FONT_26 = new Font("roman", Convert.ToInt32(Convert.ToDouble(config["DEFAULT"]["ZOOM"]) * 26), FontStyle.Bold);
            f.Left = (int)Convert.ToDouble(config["DEFAULT"]["X"]);
            //debug_print(@"config[""DEFAULT""][""Y""]:"+config["DEFAULT"]["Y"]);
            f.Top = (int)Convert.ToDouble(config["DEFAULT"]["Y"]);

            f.Opacity = Convert.ToDouble(config["DEFAULT"]["ALPHA"]);
            f.Width = 10;
            f.Height = 10;

            //Control c_type_label = f.LP.GetControlFromPosition(3, 1);
            f.LP.CellBorderStyle = System.Windows.Forms.TableLayoutPanelCellBorderStyle.Inset;
            f.LP.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.LP.AutoSize = true;
            f.LP.Width = 10;
            f.LP.Height = 10;
            f.LP.RowStyles[0] = new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
            //btn_UCL
            f.LP.ColumnStyles[0] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
            //btn_HALF
            f.LP.ColumnStyles[1] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));

            //btn_gamemode
            if (config["DEFAULT"]["SHORT_MODE"] == "1")
            {
                //短
                //tape_label
                f.LP.ColumnStyles[2] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 0);
                //word_label
                f.LP.ColumnStyles[3] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 0);
                //btn_gamemode
                f.LP.ColumnStyles[5] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 0);

            }
            else
            {
                //tape_label
                f.LP.ColumnStyles[2] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(150 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
                //word_label
                f.LP.ColumnStyles[3] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(350 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
                //btn_gamemode
                f.LP.ColumnStyles[5] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(120 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
            }

            //殘/正
            if (is_simple())
            {
                //殘模式
                f.LP.ColumnStyles[4] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
            }
            else
            {
                //正模式
                f.LP.ColumnStyles[4] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 0);
            }


            //btn_X
            f.LP.ColumnStyles[6] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));

            // 肥
            f.btn_UCL.Font = GUI_FONT_16;
            f.btn_UCL.FlatAppearance.BorderSize = 0;
            f.btn_UCL.Margin = new System.Windows.Forms.Padding(0);
            f.btn_UCL.Padding = new System.Windows.Forms.Padding(0);
            f.btn_UCL.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_UCL.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 半全
            f.btn_HALF.Font = GUI_FONT_16;
            f.btn_HALF.FlatAppearance.BorderSize = 0;
            f.btn_HALF.Margin = new System.Windows.Forms.Padding(0);
            f.btn_HALF.Padding = new System.Windows.Forms.Padding(0);
            f.btn_HALF.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_HALF.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 輸五
            f.type_label.Font = GUI_FONT_18;

            f.type_label.Margin = new System.Windows.Forms.Padding(0);
            f.type_label.Padding = new System.Windows.Forms.Padding(0);
            f.type_label.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.type_label.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 字
            f.word_label.Font = GUI_FONT_18;
            f.word_label.Margin = new System.Windows.Forms.Padding(0);
            f.word_label.Padding = new System.Windows.Forms.Padding(0);
            f.word_label.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.word_label.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 簡
            f.btn_simple.Font = GUI_FONT_18;
            f.btn_simple.Margin = new System.Windows.Forms.Padding(0);
            f.btn_simple.Padding = new System.Windows.Forms.Padding(0);
            f.btn_simple.FlatAppearance.BorderSize = 0;
            f.btn_simple.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_simple.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 遊戲
            f.btn_gamemode.Font = GUI_FONT_14;
            f.btn_gamemode.Margin = new System.Windows.Forms.Padding(0);
            f.btn_gamemode.Padding = new System.Windows.Forms.Padding(0);
            f.btn_gamemode.FlatAppearance.BorderSize = 0;
            f.btn_gamemode.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_gamemode.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // X
            f.btn_X.Font = GUI_FONT_16;
            f.btn_X.Margin = new System.Windows.Forms.Padding(0);
            f.btn_X.Padding = new System.Windows.Forms.Padding(0);
            f.btn_X.FlatAppearance.BorderSize = 0;
            f.btn_X.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_X.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            f.LP.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;
            f.LP.Dock = DockStyle.Fill;
            //f.LP.MaximumSize = new Size(200, 50);
            f.LP.AutoSize = true;
            f.LP.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_UCL.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_HALF.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_gamemode.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_X.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.type_label.AutoSize = true;
            f.word_label.AutoSize = true;
            f.Visible = true;
            //f.Refresh();
            f.ResumeLayout();
        }
        public void show_sp_to_label(string data)
        {

        }
        public void use_pinyi(string data)
        {

        }
        public bool is_ucl()
        {
            return flag_is_ucl; // (f.btn_UCL.Text == "肥");
        }
        public bool is_hf()
        {
            return flag_is_hf;// (f.btn_HALF.Text == "半");
        }
        public string widen(string data)
        {
            //半形轉全形
            data = ToWide(data);
            return data;
        }
        public void play_ucl(string thekey)
        {
            play_ucl_label = f.type_label.Text;
            //# 不可以超過5個字
            if (play_ucl_label.Length < 5)
            {
                play_ucl_label = string.Format("{0}{1}", play_ucl_label, thekey);
                type_label_set_text();
            }

        }
        public void toggle_gamemode()
        {
            string kind = f.btn_gamemode.Text;
            switch (kind)
            {
                case "正常模式":
                    flag_is_gamemode = true;
                    f.btn_gamemode.Text = "遊戲模式";
                    if (f.btn_UCL.Text == "肥")
                    {
                        toggle_ucl();
                    }
                    break;
                case "遊戲模式":
                    flag_is_gamemode = false;
                    f.btn_gamemode.Text = "正常模式";
                    break;
            }
            toAlphaOrNonAlpha();
        }
        public void toggle_hf()
        {
            string kind = f.btn_HALF.Text;
            switch (kind)
            {
                case "半":
                    f.btn_HALF.Text = "全";
                    flag_is_hf = false;
                    break;
                default:
                    f.btn_HALF.Text = "半";
                    flag_is_hf = true;
                    break;
            }
            //hf_label = self.get_child()
            //hf_label.modify_font(pango.FontDescription(GUI_FONT_22))
            toAlphaOrNonAlpha();
        }
        public void toAlphaOrNonAlpha()
        {
            // # 偵測肥米的位置，超出螢幕時，彈回
            int screen_width = Screen.PrimaryScreen.Bounds.Width;
            int screen_height = Screen.PrimaryScreen.Bounds.Height;
            int _x = f.Left;
            int _y = f.Top;
            int _width = f.Width;
            int _height = f.Height;
            int new_position_x = _x;
            int new_position_y = _y;
            if (_x > screen_width - _width)
            {
                new_position_x = screen_width - _width - 20;
                f.Left = new_position_x;
                f.Top = new_position_y;
            }
            if (_y > screen_height - _height - 40)
            {
                new_position_y = screen_height - _height - 40;
                f.Left = new_position_x;
                f.Top = new_position_y;
            }
            if (_x < 0)
            {
                new_position_x = 0;
                f.Left = new_position_x;
                f.Top = new_position_y;
            }
            if (_y < 0)
            {
                new_position_y = 0;
                f.Left = new_position_x;
                f.Top = new_position_y;
            }
            if (flag_is_ucl || !flag_is_hf)
            {
                //肥 或是 全形
                f.Opacity = Convert.ToDouble(config["DEFAULT"]["ALPHA"]);
                debug_print("Opacity:" + f.Opacity.ToString());
                f.TopMost = true;
                //f.TopLevel = true; //if open , lost focus 
            }
            else
            {
                f.Opacity = 0.2;
                f.TopMost = false;
                //f.TopLevel = false; //if open , lost focus 
            }
        }
        public void toggle_ucl()
        {
            try
            {
                switch (flag_is_ucl)
                {
                    case true:
                        f.btn_UCL.Text = "英";
                        play_ucl_label = "";
                        type_label_set_text();
                        flag_is_ucl = false;
                        break;
                    case false:
                        f.btn_UCL.Text = "肥";
                        flag_is_ucl = true;
                        break;
                }
                //debug_print("window_state_event_cb(toggle_ucl)");
                toAlphaOrNonAlpha();
                //f.Refresh();
            }
            catch (Exception ex)
            {
                debug_print("Crash..."+ex.Message);
            }
        }
        public void debug_print(string data)
        {
            if (is_DEBUG_mode)
            {
                Console.WriteLine(data);
            }
        }
        public bool type_label_set_text(string last_word_label_txt = "")
        {
            f.type_label.Text = play_ucl_label;
            if (play_ucl_label.Length > 0)
            {
                debug_print("ShowSearch");
                show_search();
            }
            else
            {
                f.word_label.Text = "";
            }
            // 如果 last_word_label_txt 不是空值，代表有簡根或其他用字
            //word_label.modify_fg(gtk.STATE_NORMAL, gtk.gdk.color_parse('black'))
            f.word_label.ForeColor = Color.Black;
            if (last_word_label_txt != "")
            {
                f.word_label.Text = last_word_label_txt;
                f.word_label.ForeColor = Color.FromArgb(0, 127, 255);
                //word_label.modify_fg(gtk.STATE_NORMAL, gtk.gdk.Color("#007fff"));
            }
            //如果是短米，自動看幾個字展長
            if (config["DEFAULT"]["SHORT_MODE"] == "1")
            {
                string _tape_label = f.type_label.Text;
                int _len_tape_label = _tape_label.Length;
                //# 一字30
                if (_len_tape_label == 0)
                {
                    //f.type_label.Visible = false;
                    f.LP.ColumnStyles[2] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 0 * (int)Convert.ToDouble(config["DEFAULT"]["ZOOM"]));
                }
                else
                {
                    //f.type_label.Visible = true;
                }
                //f.type_label.set_size_request(int(float(config['DEFAULT']['zoom']) * 18 * _len_tape_label), int(float(config['DEFAULT']['zoom']) * 40))                
                f.LP.ColumnStyles[2] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute,
                    (int)(28 * _len_tape_label * Convert.ToDouble(config["DEFAULT"]["ZOOM"]))
                );
                string _word_label = f.word_label.Text;
                int _len_word_label = _word_label.Length;
                //#一字30
                if (_len_word_label == 0)
                {
                    //f.word_label.Visible = false;
                }
                else
                {
                    // f.word_label.Visible = true;
                }
                //f.word_label.set_size_request(int(float(config['DEFAULT']['zoom']) * 15 * _len_word_label), int(float(config['DEFAULT']['zoom']) * 40))
                f.LP.ColumnStyles[3] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute,
                                    (int)(28 * _len_word_label * Convert.ToDouble(config["DEFAULT"]["ZOOM"]))
                                );
            }
            return true;
        }
        public bool show_search()
        {
            //#真的要顯示了
            same_sound_index = 0;
            is_has_more_page = false;
            same_sound_last_word = "";
            debug_print("ShowSearch1");
            string c = play_ucl_label.ToLower().Trim();
            is_need_use_pinyi = false;
            if (c.Substring(0, 1) == "'" && c.Length > 1)
            {
                c = c.Substring(1);
                is_need_use_pinyi = true;
            }
            if (!my.in_array(c, uclcode["chardefs"]) && c.Substring(c.Length - 1, 1) == "v" && my.in_array(c.Substring(0, c.Length - 1), uclcode["chardefs"]) && uclcode["chardefs"][c.Substring(0, c.Length - 1)].Count >= 2)
            {
                //#print("Debug V1")
                ucl_find_data = my.jsonValueToListString(uclcode["chardefs"][c.Substring(0, c.Length - 1)][1]);
                word_label_set_text();
                return true;
            }
            else if (!my.in_array(c, uclcode["chardefs"]) && c.Substring(c.Length - 1, 1) == "r" && my.in_array(c.Substring(0, c.Length - 1), uclcode["chardefs"]) && uclcode["chardefs"][c.Substring(0, c.Length - 1)].Count >= 3)
            {
                //#print("Debug V1")
                ucl_find_data = my.jsonValueToListString(uclcode["chardefs"][c.Substring(0, c.Length - 1)][2]);
                word_label_set_text();
                return true;
            }
            else if (!my.in_array(c, uclcode["chardefs"]) && c.Substring(c.Length - 1, 1) == "s" && my.in_array(c.Substring(0, c.Length - 1), uclcode["chardefs"]) && uclcode["chardefs"][c.Substring(0, c.Length - 1)].Count >= 4)
            {
                //#print("Debug V1")
                ucl_find_data = my.jsonValueToListString(uclcode["chardefs"][c.Substring(0, c.Length - 1)][3]);
                word_label_set_text();
                return true;
            }
            else if (!my.in_array(c, uclcode["chardefs"]) && c.Substring(c.Length - 1, 1) == "f" && my.in_array(c.Substring(0, c.Length - 1), uclcode["chardefs"]) && uclcode["chardefs"][c.Substring(0, c.Length - 1)].Count >= 5)
            {
                //#print("Debug V1")
                ucl_find_data = my.jsonValueToListString(uclcode["chardefs"][c.Substring(0, c.Length - 1)][4]);
                word_label_set_text();
                return true;
            }
            else if (my.in_array(c, uclcode["chardefs"]))
            {
                //# print("Debug V2")
                ucl_find_data = my.jsonValueToListString(uclcode["chardefs"][c]);
                word_label_set_text();
                return true;
            }
            else
            {
                ucl_find_data = new List<string>();
                word_label_set_text();
                return false;
            }
        }
        public bool word_label_set_text()
        {
            if (play_ucl_label == "")
            {
                f.word_label.Text = "";
                //word_label.modify_font(pango.FontDescription(GUI_FONT_18))
                return true;
            }
            int step = 0;
            string tmp = "";
            List<string> m = new List<string>();
            //MessageBox.Show(f.word_label.Visible.ToString());
            //f.word_label.Text = "BBB";

            try
            {
                foreach (var k in ucl_find_data)
                {
                    m.Add(string.Format("{0}{1}", step, k));
                    step = step + 1;
                }
                tmp = my.implode(" ", m);
                if (is_has_more_page == true)
                {
                    tmp = string.Format("{0} ...", tmp);
                }
                f.word_label.Text = tmp;
                debug_print(string.Format("word_label lens: {0} ", tmp.Length));
                int lt = tmp.Length;
            }
            catch (Exception ex)
            {
                debug_print("word_label_set_text exception: " + ex.Message);
                play_ucl_label = "";
                play_ucl("");
                //f.word_label.Text = "";
                //word_label.modify_font(pango.FontDescription(GUI_FONT_18))                
            }
            return true;
        }
        public void toggle_half()
        {
            switch (f.btn_HALF.Text)
            {
                case "半":
                    f.btn_HALF.Text = "全";
                    break;
                case "全":
                    f.btn_HALF.Text = "半";
                    break;
            }        
        }
        public Dictionary<string, string> getForegroundWindowProcessInfo()
        {
            //取得當前使用環境APP細節
            //回傳 
            //PROCESS_TITLE 標題
            //PROCESS_NAME 檔名
            //PROCESS_PID 編號
            //https://stackoverflow.com/questions/115868/how-do-i-get-the-title-of-the-current-active-window-using-c
            //From : https://stackoverflow.com/questions/115868/how-do-i-get-the-title-of-the-current-active-window-using-c                
            // Try : https://github.com/ReneLergner/WPinternals/blob/master/CommandLine.cs
            //Microsoft.Win32.SafeHandles.SafeFileHandle safeFileHandle = new Microsoft.Win32.SafeHandles.SafeFileHandle(handle, true);
            //FileStream fileStream = new FileStream(safeFileHandle, FileAccess.Write);
            //fileStream.
            //Encoding encoding = System.Text.Encoding.GetEncoding(MY_CODE_PAGE);
            IntPtr handle = Form1.GetForegroundWindow();
            var intLength = Form1.GetWindowTextLength(handle) + 1;
            StringBuilder Buff = new StringBuilder(intLength);
            if (Form1.GetWindowText(handle, Buff, intLength) > 0)
            {
                // return Buff.ToString();
                //debug_print("BBBBBBBBBBBBBBBB:" + Buff.ToString());
            }
            string Proc_TITLE = Buff.ToString();
            uint Proc_PID;
            Form1.GetWindowThreadProcessId(handle, out Proc_PID);
            Process p = Process.GetProcessById((int)Proc_PID);
            string Proc_NAME = "";
            try
            {
                //某些 app 會當，如 skype
                Proc_NAME = my.mainname(p.MainModule.FileName.ToLower());
            }
            catch (Exception ex)
            {

                debug_print("Get ProcName failure:" + ex.Message);
            }
            Dictionary<string, string> output = new Dictionary<string, string>();
            //PROCESS_TITLE 標題
            //PROCESS_NAME 檔名
            //PROCESS_PID 編號
            output["PROCESS_TITLE"] = Proc_TITLE;
            output["PROCESS_NAME"] = Proc_NAME;
            output["PROCESS_PID"] = Proc_PID.ToString();
            return output;
        }

        public void senddata(string data)
        {
            //人生很難，研究很久 C# 的 sendkeys 遇到有些吃 iso-8859-1、big5 的app 如pcman、putty
            //或是早期的 photoimpact，最好的方法還是利用剪貼簿貼上，使用前備份一下原來的內容即可
            //sendinput 似乎也是一個解法，但有空再來研究
            //data = "肥的天下";
            same_sound_index = 0;// #回到第零頁
            is_has_more_page = false;// #回到沒有分頁
            same_sound_last_word = "";
            play_ucl_label = "";
            ucl_find_data = new List<string>();
            type_label_set_text();

            if (is_simple())
            {
                //如果需要殘體，正轉殘
                data = trad2simple(data);
            }

            if (data == "")
            {
                is_send_ucl = false;
                debug_print("debug senddata empty");
                return;
            }

            //出字
            //From : https://burorly.pixnet.net/blog/post/10185692-c%23%E8%A7%A3%E6%B1%BA%E4%B8%AD%E6%96%87%E5%AD%97%E5%8D%A0%E7%94%A82%E5%80%8Bbyte%E9%95%B7%E5%BA%A6%E8%A8%88%E7%AE%97%E6%96%B9%E5%BC%8F
            //byte[] lineStr = System.Text.Encoding.UTF8.GetBytes(data);
            //int len = System.Text.Encoding.UTF8.GetByteCount(data);
            //{
            //
            //  string str = data.Substring(i, 1); 
            //sendinput
            //https://dotblogs.com.tw/eaglewolf/2010/10/08/18220
            //https://www.itread01.com/content/1548344359.html
            /*
                更多舉例:
                SendKeys.SendWait("^C");  //Ctrl+C 組合鍵
                SendKeys.SendWait("+C");  //Shift+C 組合鍵
                SendKeys.SendWait("%C");  //Alt+C 組合鍵
                SendKeys.SendWait("+(AX)");  //Shift+A+X 組合鍵
                SendKeys.SendWait("+AX");  //Shift+A 組合鍵,之後按X鍵
                SendKeys.SendWait("{left 5}");  //按←鍵 5次
                SendKeys.SendWait("{h 10}");   //按h鍵 10次
                SendKeys.Send("漢字");  //模擬輸入"漢字"2個字
            */

            //output += str;
            //Thread.Sleep(50);
            //}

            debug_print("Sendkeys:" + data);

            var p_info = getForegroundWindowProcessInfo();
            if (my.in_array(p_info["PROCESS_NAME"], sendkey_paste_shift_ins_apps))
            {
                //使用 shift+insert 出字
                string orin_Clip = Clipboard.GetText();
                Clipboard.SetText(data);
                is_send_ucl = true;
                data = "+{INSERT}";
                SendKeys.Send(data);
                is_send_ucl = false;
                Clipboard.SetText(orin_Clip);
            }
            else if (my.in_array(p_info["PROCESS_NAME"], sendkey_paste_ctrl_v_apps))
            {
                //使用 shift+insert 出字
                string orin_Clip = Clipboard.GetText();
                Clipboard.SetText(data);
                is_send_ucl = true;
                data = "^{v}";
                SendKeys.Send(data);
                is_send_ucl = false;
                Clipboard.SetText(orin_Clip);
            }
            else if (my.in_array(p_info["PROCESS_NAME"], sendkey_paste_big5_apps))
            {
                string orin_Clip = Clipboard.GetText();
                Clipboard.SetText(my.UTF8toBig5(data));
                is_send_ucl = true;
                data = "^{v}";
                SendKeys.Send(data);
                is_send_ucl = false;
                Clipboard.SetText(orin_Clip);
            }
            else
            {
                //其他，平非的出字
                is_send_ucl = true;
                SendKeys.Send(data);
                is_send_ucl = false;
            }

        }
    }
}
