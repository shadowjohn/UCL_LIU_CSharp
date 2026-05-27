using System;
using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using System.Drawing;
using utility;
using System.Text;
using System.Diagnostics;
using System.Threading;
//using Microsoft.VisualBasic.Strings;
namespace uclliu
{
    public class uclliu
    {
        myinclude my = new myinclude();
        private readonly UnicodeSendInputOutput unicodeSendInputOutput = new UnicodeSendInputOutput();
        private readonly WindowMessageCharOutput windowMessageCharOutput = new WindowMessageCharOutput();
        private readonly ClipboardPasteOutput clipboardPasteOutput = new ClipboardPasteOutput();
        private readonly TsfBridgeOutput tsfBridgeOutput = new TsfBridgeOutput();
        private readonly SelectedTextTransformCommand selectedTextTransformCommand = new SelectedTextTransformCommand();
        private readonly SelectedTextTransformDispatcher selectedTextTransformDispatcher;
        private readonly DeferredTextOutputDispatcher deferredTextOutputDispatcher;
        private readonly UiLabelUpdateBatcher labelUpdateBatcher;
        private readonly OutputHintComposer outputHintComposer = new OutputHintComposer();
        private readonly TypingSoundPlayer typingSoundPlayer = new TypingSoundPlayer();
        private readonly KeyboardHookLatencyMonitor keyboardHookLatencyMonitor = new KeyboardHookLatencyMonitor(KeyboardHookPerformancePolicy.SlowHookThresholdMilliseconds, KeyboardHookPerformancePolicy.SlowHookLogIntervalMilliseconds);
        private readonly AsyncPerformanceLogger performanceLogger;
        public readonly TsfBridgeManager tsfBridgeManager;
        private const int ShortModeTextPadding = 8;
        public string VERSION = UclLiuAppInfo.Version;
        public FileStream lockFileString;
        public string CUSTOM_JSON_FILE
        {
            get { return my.pwd() + "\\custom.json"; }
        }

        //把 chardefs 的字碼，變成對照字根，可以加速 ,,,z、,,,x 反查的速度
        public Dictionary<string, string> uclcode_r = new Dictionary<string, string>();
        public Dictionary<string, string> uclcode_rr = new Dictionary<string, string>();
        public Dictionary<string, List<string>> uclcode = null;
        public bool is_DEBUG_mode = false; //除錯模式
        public string INI_CONFIG_FILE = "C:\\temp\\UCLLIU.ini"; //預設在 此，實際使用的位置同在 uclliu.exe
        public List<string> same_sound_data = new List<string>(); //拚音
        public string DEFAULT_OUTPUT_TYPE = "DEFAULT";
        //硬派出字方式選擇        
        //感謝台灣碼農
        private string TC_CDATA = "万与丑专业丛东丝丢两严丧个丬丰临为丽举么义乌乐乔习乡书买乱争于亏云亘亚产亩亲亵亸亿仅从仑仓仪们价众优伙会伛伞伟传伤伥伦伧伪伫体余佣佥侠侣侥侦侧侨侩侪侬俣俦俨俩俪俭债倾偬偻偾偿傥傧储傩儿兑兖党兰关兴兹养兽冁内冈册写军农冢冯冲决况冻净凄凉凌减凑凛几凤凫凭凯击凼凿刍划刘则刚创删别刬刭刽刿剀剂剐剑剥剧劝办务劢动励劲劳势勋勐勚匀匦匮区医华协单卖卢卤卧卫却卺厂厅历厉压厌厍厕厢厣厦厨厩厮县参叆叇双发变叙叠叶号叹叽吁后吓吕吗吣吨听启吴呒呓呕呖呗员呙呛呜咏咔咙咛咝咤咴咸哌响哑哒哓哔哕哗哙哜哝哟唛唝唠唡唢唣唤唿啧啬啭啮啰啴啸喷喽喾嗫呵嗳嘘嘤嘱噜噼嚣嚯团园囱围囵国图圆圣圹场坂坏块坚坛坜坝坞坟坠垄垅垆垒垦垧垩垫垭垯垱垲垴埘埙埚埝埯堑堕塆墙壮声壳壶壸处备复够头夸夹夺奁奂奋奖奥妆妇妈妩妪妫姗姜娄娅娆娇娈娱娲娴婳婴婵婶媪嫒嫔嫱嬷孙学孪宁宝实宠审宪宫宽宾寝对寻导寿将尔尘尧尴尸尽层屃屉届属屡屦屿岁岂岖岗岘岙岚岛岭岳岽岿峃峄峡峣峤峥峦崂崃崄崭嵘嵚嵛嵝嵴巅巩巯币帅师帏帐帘帜带帧帮帱帻帼幂幞干并广庄庆庐庑库应庙庞废庼廪开异弃张弥弪弯弹强归当录彟彦彻径徕御忆忏忧忾怀态怂怃怄怅怆怜总怼怿恋恳恶恸恹恺恻恼恽悦悫悬悭悯惊惧惨惩惫惬惭惮惯愍愠愤愦愿慑慭憷懑懒懔戆戋戏戗战戬户扎扑扦执扩扪扫扬扰抚抛抟抠抡抢护报担拟拢拣拥拦拧拨择挂挚挛挜挝挞挟挠挡挢挣挤挥挦捞损捡换捣据捻掳掴掷掸掺掼揸揽揿搀搁搂搅携摄摅摆摇摈摊撄撑撵撷撸撺擞攒敌敛数斋斓斗斩断无旧时旷旸昙昼昽显晋晒晓晔晕晖暂暧札术朴机杀杂权条来杨杩杰极构枞枢枣枥枧枨枪枫枭柜柠柽栀栅标栈栉栊栋栌栎栏树栖样栾桊桠桡桢档桤桥桦桧桨桩梦梼梾检棂椁椟椠椤椭楼榄榇榈榉槚槛槟槠横樯樱橥橱橹橼檐檩欢欤欧歼殁殇残殒殓殚殡殴毁毂毕毙毡毵氇气氢氩氲汇汉污汤汹沓沟没沣沤沥沦沧沨沩沪沵泞泪泶泷泸泺泻泼泽泾洁洒洼浃浅浆浇浈浉浊测浍济浏浐浑浒浓浔浕涂涌涛涝涞涟涠涡涢涣涤润涧涨涩淀渊渌渍渎渐渑渔渖渗温游湾湿溃溅溆溇滗滚滞滟滠满滢滤滥滦滨滩滪漤潆潇潋潍潜潴澜濑濒灏灭灯灵灾灿炀炉炖炜炝点炼炽烁烂烃烛烟烦烧烨烩烫烬热焕焖焘煅煳熘爱爷牍牦牵牺犊犟状犷犸犹狈狍狝狞独狭狮狯狰狱狲猃猎猕猡猪猫猬献獭玑玙玚玛玮环现玱玺珉珏珐珑珰珲琎琏琐琼瑶瑷璇璎瓒瓮瓯电画畅畲畴疖疗疟疠疡疬疮疯疱疴痈痉痒痖痨痪痫痴瘅瘆瘗瘘瘪瘫瘾瘿癞癣癫癯皑皱皲盏盐监盖盗盘眍眦眬着睁睐睑瞒瞩矫矶矾矿砀码砖砗砚砜砺砻砾础硁硅硕硖硗硙硚确硷碍碛碜碱碹磙礼祎祢祯祷祸禀禄禅离秃秆种积称秽秾稆税稣稳穑穷窃窍窑窜窝窥窦窭竖竞笃笋笔笕笺笼笾筑筚筛筜筝筹签简箓箦箧箨箩箪箫篑篓篮篱簖籁籴类籼粜粝粤粪粮糁糇紧絷纟纠纡红纣纤纥约级纨纩纪纫纬纭纮纯纰纱纲纳纴纵纶纷纸纹纺纻纼纽纾线绀绁绂练组绅细织终绉绊绋绌绍绎经绐绑绒结绔绕绖绗绘给绚绛络绝绞统绠绡绢绣绤绥绦继绨绩绪绫绬续绮绯绰绱绲绳维绵绶绷绸绹绺绻综绽绾绿缀缁缂缃缄缅缆缇缈缉缊缋缌缍缎缏缐缑缒缓缔缕编缗缘缙缚缛缜缝缞缟缠缡缢缣缤缥缦缧缨缩缪缫缬缭缮缯缰缱缲缳缴缵罂网罗罚罢罴羁羟羡翘翙翚耢耧耸耻聂聋职聍联聩聪肃肠肤肷肾肿胀胁胆胜胧胨胪胫胶脉脍脏脐脑脓脔脚脱脶脸腊腌腘腭腻腼腽腾膑臜舆舣舰舱舻艰艳艹艺节芈芗芜芦苁苇苈苋苌苍苎苏苘苹茎茏茑茔茕茧荆荐荙荚荛荜荞荟荠荡荣荤荥荦荧荨荩荪荫荬荭荮药莅莜莱莲莳莴莶获莸莹莺莼萚萝萤营萦萧萨葱蒇蒉蒋蒌蓝蓟蓠蓣蓥蓦蔷蔹蔺蔼蕲蕴薮藁藓虏虑虚虫虬虮虽虾虿蚀蚁蚂蚕蚝蚬蛊蛎蛏蛮蛰蛱蛲蛳蛴蜕蜗蜡蝇蝈蝉蝎蝼蝾螀螨蟏衅衔补衬衮袄袅袆袜袭袯装裆裈裢裣裤裥褛褴襁襕见观觃规觅视觇览觉觊觋觌觍觎觏觐觑觞触觯詟誉誊讠计订讣认讥讦讧讨让讪讫训议讯记讱讲讳讴讵讶讷许讹论讻讼讽设访诀证诂诃评诅识诇诈诉诊诋诌词诎诏诐译诒诓诔试诖诗诘诙诚诛诜话诞诟诠诡询诣诤该详诧诨诩诪诫诬语诮误诰诱诲诳说诵诶请诸诹诺读诼诽课诿谀谁谂调谄谅谆谇谈谊谋谌谍谎谏谐谑谒谓谔谕谖谗谘谙谚谛谜谝谞谟谠谡谢谣谤谥谦谧谨谩谪谫谬谭谮谯谰谱谲谳谴谵谶谷豮贝贞负贠贡财责贤败账货质贩贪贫贬购贮贯贰贱贲贳贴贵贶贷贸费贺贻贼贽贾贿赀赁赂赃资赅赆赇赈赉赊赋赌赍赎赏赐赑赒赓赔赕赖赗赘赙赚赛赜赝赞赟赠赡赢赣赪赵赶趋趱趸跃跄跖跞践跶跷跸跹跻踊踌踪踬踯蹑蹒蹰蹿躏躜躯车轧轨轩轪轫转轭轮软轰轱轲轳轴轵轶轷轸轹轺轻轼载轾轿辀辁辂较辄辅辆辇辈辉辊辋辌辍辎辏辐辑辒输辔辕辖辗辘辙辚辞辩辫边辽达迁过迈运还这进远违连迟迩迳迹适选逊递逦逻遗遥邓邝邬邮邹邺邻郁郄郏郐郑郓郦郧郸酝酦酱酽酾酿释里鉅鉴銮錾钆钇针钉钊钋钌钍钎钏钐钑钒钓钔钕钖钗钘钙钚钛钝钞钟钠钡钢钣钤钥钦钧钨钩钪钫钬钭钮钯钰钱钲钳钴钵钶钷钸钹钺钻钼钽钾钿铀铁铂铃铄铅铆铈铉铊铋铍铎铏铐铑铒铕铗铘铙铚铛铜铝铞铟铠铡铢铣铤铥铦铧铨铪铫铬铭铮铯铰铱铲铳铴铵银铷铸铹铺铻铼铽链铿销锁锂锃锄锅锆锇锈锉锊锋锌锍锎锏锐锑锒锓锔锕锖锗错锚锜锞锟锠锡锢锣锤锥锦锨锩锫锬锭键锯锰锱锲锳锴锵锶锷锸锹锺锻锼锽锾锿镀镁镂镃镆镇镈镉镊镌镍镎镏镐镑镒镕镖镗镙镚镛镜镝镞镟镠镡镢镣镤镥镦镧镨镩镪镫镬镭镮镯镰镱镲镳镴镶长门闩闪闫闬闭问闯闰闱闲闳间闵闶闷闸闹闺闻闼闽闾闿阀阁阂阃阄阅阆阇阈阉阊阋阌阍阎阏阐阑阒阓阔阕阖阗阘阙阚阛队阳阴阵阶际陆陇陈陉陕陧陨险随隐隶隽难雏雠雳雾霁霉霭靓静靥鞑鞒鞯鞴韦韧韨韩韪韫韬韵页顶顷顸项顺须顼顽顾顿颀颁颂颃预颅领颇颈颉颊颋颌颍颎颏颐频颒颓颔颕颖颗题颙颚颛颜额颞颟颠颡颢颣颤颥颦颧风飏飐飑飒飓飔飕飖飗飘飙飚飞飨餍饤饥饦饧饨饩饪饫饬饭饮饯饰饱饲饳饴饵饶饷饸饹饺饻饼饽饾饿馀馁馂馃馄馅馆馇馈馉馊馋馌馍馎馏馐馑馒馓馔馕马驭驮驯驰驱驲驳驴驵驶驷驸驹驺驻驼驽驾驿骀骁骂骃骄骅骆骇骈骉骊骋验骍骎骏骐骑骒骓骔骕骖骗骘骙骚骛骜骝骞骟骠骡骢骣骤骥骦骧髅髋髌鬓魇魉鱼鱽鱾鱿鲀鲁鲂鲄鲅鲆鲇鲈鲉鲊鲋鲌鲍鲎鲏鲐鲑鲒鲓鲔鲕鲖鲗鲘鲙鲚鲛鲜鲝鲞鲟鲠鲡鲢鲣鲤鲥鲦鲧鲨鲩鲪鲫鲬鲭鲮鲯鲰鲱鲲鲳鲴鲵鲶鲷鲸鲹鲺鲻鲼鲽鲾鲿鳀鳁鳂鳃鳄鳅鳆鳇鳈鳉鳊鳋鳌鳍鳎鳏鳐鳑鳒鳓鳔鳕鳖鳗鳘鳙鳛鳜鳝鳞鳟鳠鳡鳢鳣鸟鸠鸡鸢鸣鸤鸥鸦鸧鸨鸩鸪鸫鸬鸭鸮鸯鸰鸱鸲鸳鸴鸵鸶鸷鸸鸹鸺鸻鸼鸽鸾鸿鹀鹁鹂鹃鹄鹅鹆鹇鹈鹉鹊鹋鹌鹍鹎鹏鹐鹑鹒鹓鹔鹕鹖鹗鹘鹚鹛鹜鹝鹞鹟鹠鹡鹢鹣鹤鹥鹦鹧鹨鹩鹪鹫鹬鹭鹯鹰鹱鹲鹳鹴鹾麦麸黄黉黡黩黪黾鼋鼌鼍鼗鼹齄齐齑齿龀龁龂龃龄龅龆龇龈龉龊龋龌龙龚龛龟志制咨只里系范松没尝尝闹面准钟别闲干尽脏拼";
        private string TC_TDATA = "萬與醜專業叢東絲丟兩嚴喪個爿豐臨為麗舉麼義烏樂喬習鄉書買亂爭於虧雲亙亞產畝親褻嚲億僅從侖倉儀們價眾優夥會傴傘偉傳傷倀倫傖偽佇體餘傭僉俠侶僥偵側僑儈儕儂俁儔儼倆儷儉債傾傯僂僨償儻儐儲儺兒兌兗黨蘭關興茲養獸囅內岡冊寫軍農塚馮衝決況凍淨淒涼淩減湊凜幾鳳鳧憑凱擊氹鑿芻劃劉則剛創刪別剗剄劊劌剴劑剮劍剝劇勸辦務勱動勵勁勞勢勳猛勩勻匭匱區醫華協單賣盧鹵臥衛卻巹廠廳曆厲壓厭厙廁廂厴廈廚廄廝縣參靉靆雙發變敘疊葉號歎嘰籲後嚇呂嗎唚噸聽啟吳嘸囈嘔嚦唄員咼嗆嗚詠哢嚨嚀噝吒噅鹹呱響啞噠嘵嗶噦嘩噲嚌噥喲嘜嗊嘮啢嗩唕喚呼嘖嗇囀齧囉嘽嘯噴嘍嚳囁嗬噯噓嚶囑嚕劈囂謔團園囪圍圇國圖圓聖壙場阪壞塊堅壇壢壩塢墳墜壟壟壚壘墾坰堊墊埡墶壋塏堖塒塤堝墊垵塹墮壪牆壯聲殼壺壼處備複夠頭誇夾奪奩奐奮獎奧妝婦媽嫵嫗媯姍薑婁婭嬈嬌孌娛媧嫻嫿嬰嬋嬸媼嬡嬪嬙嬤孫學孿寧寶實寵審憲宮寬賓寢對尋導壽將爾塵堯尷屍盡層屭屜屆屬屢屨嶼歲豈嶇崗峴嶴嵐島嶺嶽崠巋嶨嶧峽嶢嶠崢巒嶗崍嶮嶄嶸嶔崳嶁脊巔鞏巰幣帥師幃帳簾幟帶幀幫幬幘幗冪襆幹並廣莊慶廬廡庫應廟龐廢廎廩開異棄張彌弳彎彈強歸當錄彠彥徹徑徠禦憶懺憂愾懷態慫憮慪悵愴憐總懟懌戀懇惡慟懨愷惻惱惲悅愨懸慳憫驚懼慘懲憊愜慚憚慣湣慍憤憒願懾憖怵懣懶懍戇戔戲戧戰戩戶紮撲扡執擴捫掃揚擾撫拋摶摳掄搶護報擔擬攏揀擁攔擰撥擇掛摯攣掗撾撻挾撓擋撟掙擠揮撏撈損撿換搗據撚擄摑擲撣摻摜摣攬撳攙擱摟攪攜攝攄擺搖擯攤攖撐攆擷擼攛擻攢敵斂數齋斕鬥斬斷無舊時曠暘曇晝曨顯晉曬曉曄暈暉暫曖劄術樸機殺雜權條來楊榪傑極構樅樞棗櫪梘棖槍楓梟櫃檸檉梔柵標棧櫛櫳棟櫨櫟欄樹棲樣欒棬椏橈楨檔榿橋樺檜槳樁夢檮棶檢欞槨櫝槧欏橢樓欖櫬櫚櫸檟檻檳櫧橫檣櫻櫫櫥櫓櫞簷檁歡歟歐殲歿殤殘殞殮殫殯毆毀轂畢斃氈毿氌氣氫氬氳匯漢汙湯洶遝溝沒灃漚瀝淪滄渢溈滬濔濘淚澩瀧瀘濼瀉潑澤涇潔灑窪浹淺漿澆湞溮濁測澮濟瀏滻渾滸濃潯濜塗湧濤澇淶漣潿渦溳渙滌潤澗漲澀澱淵淥漬瀆漸澠漁瀋滲溫遊灣濕潰濺漵漊潷滾滯灩灄滿瀅濾濫灤濱灘澦濫瀠瀟瀲濰潛瀦瀾瀨瀕灝滅燈靈災燦煬爐燉煒熗點煉熾爍爛烴燭煙煩燒燁燴燙燼熱煥燜燾煆糊溜愛爺牘犛牽犧犢強狀獷獁猶狽麅獮獰獨狹獅獪猙獄猻獫獵獼玀豬貓蝟獻獺璣璵瑒瑪瑋環現瑲璽瑉玨琺瓏璫琿璡璉瑣瓊瑤璦璿瓔瓚甕甌電畫暢佘疇癤療瘧癘瘍鬁瘡瘋皰屙癰痙癢瘂癆瘓癇癡癉瘮瘞瘺癟癱癮癭癩癬癲臒皚皺皸盞鹽監蓋盜盤瞘眥矓著睜睞瞼瞞矚矯磯礬礦碭碼磚硨硯碸礪礱礫礎硜矽碩硤磽磑礄確鹼礙磧磣堿镟滾禮禕禰禎禱禍稟祿禪離禿稈種積稱穢穠穭稅穌穩穡窮竊竅窯竄窩窺竇窶豎競篤筍筆筧箋籠籩築篳篩簹箏籌簽簡籙簀篋籜籮簞簫簣簍籃籬籪籟糴類秈糶糲粵糞糧糝餱緊縶糸糾紆紅紂纖紇約級紈纊紀紉緯紜紘純紕紗綱納紝縱綸紛紙紋紡紵紖紐紓線紺絏紱練組紳細織終縐絆紼絀紹繹經紿綁絨結絝繞絰絎繪給絢絳絡絕絞統綆綃絹繡綌綏絛繼綈績緒綾緓續綺緋綽緔緄繩維綿綬繃綢綯綹綣綜綻綰綠綴緇緙緗緘緬纜緹緲緝縕繢緦綞緞緶線緱縋緩締縷編緡緣縉縛縟縝縫縗縞纏縭縊縑繽縹縵縲纓縮繆繅纈繚繕繒韁繾繰繯繳纘罌網羅罰罷羆羈羥羨翹翽翬耮耬聳恥聶聾職聹聯聵聰肅腸膚膁腎腫脹脅膽勝朧腖臚脛膠脈膾髒臍腦膿臠腳脫腡臉臘醃膕齶膩靦膃騰臏臢輿艤艦艙艫艱豔艸藝節羋薌蕪蘆蓯葦藶莧萇蒼苧蘇檾蘋莖蘢蔦塋煢繭荊薦薘莢蕘蓽蕎薈薺蕩榮葷滎犖熒蕁藎蓀蔭蕒葒葤藥蒞蓧萊蓮蒔萵薟獲蕕瑩鶯蓴蘀蘿螢營縈蕭薩蔥蕆蕢蔣蔞藍薊蘺蕷鎣驀薔蘞藺藹蘄蘊藪槁蘚虜慮虛蟲虯蟣雖蝦蠆蝕蟻螞蠶蠔蜆蠱蠣蟶蠻蟄蛺蟯螄蠐蛻蝸蠟蠅蟈蟬蠍螻蠑螿蟎蠨釁銜補襯袞襖嫋褘襪襲襏裝襠褌褳襝褲襇褸襤繈襴見觀覎規覓視覘覽覺覬覡覿覥覦覯覲覷觴觸觶讋譽謄訁計訂訃認譏訐訌討讓訕訖訓議訊記訒講諱謳詎訝訥許訛論訩訟諷設訪訣證詁訶評詛識詗詐訴診詆謅詞詘詔詖譯詒誆誄試詿詩詰詼誠誅詵話誕詬詮詭詢詣諍該詳詫諢詡譸誡誣語誚誤誥誘誨誑說誦誒請諸諏諾讀諑誹課諉諛誰諗調諂諒諄誶談誼謀諶諜謊諫諧謔謁謂諤諭諼讒諮諳諺諦謎諞諝謨讜謖謝謠謗諡謙謐謹謾謫譾謬譚譖譙讕譜譎讞譴譫讖穀豶貝貞負貟貢財責賢敗賬貨質販貪貧貶購貯貫貳賤賁貰貼貴貺貸貿費賀貽賊贄賈賄貲賃賂贓資賅贐賕賑賚賒賦賭齎贖賞賜贔賙賡賠賧賴賵贅賻賺賽賾贗讚贇贈贍贏贛赬趙趕趨趲躉躍蹌蹠躒踐躂蹺蹕躚躋踴躊蹤躓躑躡蹣躕躥躪躦軀車軋軌軒軑軔轉軛輪軟轟軲軻轤軸軹軼軤軫轢軺輕軾載輊轎輈輇輅較輒輔輛輦輩輝輥輞輬輟輜輳輻輯轀輸轡轅轄輾轆轍轔辭辯辮邊遼達遷過邁運還這進遠違連遲邇逕跡適選遜遞邐邏遺遙鄧鄺鄔郵鄒鄴鄰鬱郤郟鄶鄭鄆酈鄖鄲醞醱醬釅釃釀釋裏钜鑒鑾鏨釓釔針釘釗釙釕釷釺釧釤鈒釩釣鍆釹鍚釵鈃鈣鈈鈦鈍鈔鍾鈉鋇鋼鈑鈐鑰欽鈞鎢鉤鈧鈁鈥鈄鈕鈀鈺錢鉦鉗鈷缽鈳鉕鈽鈸鉞鑽鉬鉭鉀鈿鈾鐵鉑鈴鑠鉛鉚鈰鉉鉈鉍鈹鐸鉶銬銠鉺銪鋏鋣鐃銍鐺銅鋁銱銦鎧鍘銖銑鋌銩銛鏵銓鉿銚鉻銘錚銫鉸銥鏟銃鐋銨銀銣鑄鐒鋪鋙錸鋱鏈鏗銷鎖鋰鋥鋤鍋鋯鋨鏽銼鋝鋒鋅鋶鐦鐧銳銻鋃鋟鋦錒錆鍺錯錨錡錁錕錩錫錮鑼錘錐錦鍁錈錇錟錠鍵鋸錳錙鍥鍈鍇鏘鍶鍔鍤鍬鍾鍛鎪鍠鍰鎄鍍鎂鏤鎡鏌鎮鎛鎘鑷鐫鎳鎿鎦鎬鎊鎰鎔鏢鏜鏍鏰鏞鏡鏑鏃鏇鏐鐔钁鐐鏷鑥鐓鑭鐠鑹鏹鐙鑊鐳鐶鐲鐮鐿鑔鑣鑞鑲長門閂閃閆閈閉問闖閏闈閑閎間閔閌悶閘鬧閨聞闥閩閭闓閥閣閡閫鬮閱閬闍閾閹閶鬩閿閽閻閼闡闌闃闠闊闋闔闐闒闕闞闤隊陽陰陣階際陸隴陳陘陝隉隕險隨隱隸雋難雛讎靂霧霽黴靄靚靜靨韃鞽韉韝韋韌韍韓韙韞韜韻頁頂頃頇項順須頊頑顧頓頎頒頌頏預顱領頗頸頡頰頲頜潁熲頦頤頻頮頹頷頴穎顆題顒顎顓顏額顳顢顛顙顥纇顫顬顰顴風颺颭颮颯颶颸颼颻飀飄飆飆飛饗饜飣饑飥餳飩餼飪飫飭飯飲餞飾飽飼飿飴餌饒餉餄餎餃餏餅餑餖餓餘餒餕餜餛餡館餷饋餶餿饞饁饃餺餾饈饉饅饊饌饢馬馭馱馴馳驅馹駁驢駔駛駟駙駒騶駐駝駑駕驛駘驍罵駰驕驊駱駭駢驫驪騁驗騂駸駿騏騎騍騅騌驌驂騙騭騤騷騖驁騮騫騸驃騾驄驏驟驥驦驤髏髖髕鬢魘魎魚魛魢魷魨魯魴魺鮁鮃鯰鱸鮋鮓鮒鮊鮑鱟鮍鮐鮭鮚鮳鮪鮞鮦鰂鮜鱠鱭鮫鮮鮺鯗鱘鯁鱺鰱鰹鯉鰣鰷鯀鯊鯇鮶鯽鯒鯖鯪鯕鯫鯡鯤鯧鯝鯢鯰鯛鯨鯵鯴鯔鱝鰈鰏鱨鯷鰮鰃鰓鱷鰍鰒鰉鰁鱂鯿鰠鼇鰭鰨鰥鰩鰟鰜鰳鰾鱈鱉鰻鰵鱅鰼鱖鱔鱗鱒鱯鱤鱧鱣鳥鳩雞鳶鳴鳲鷗鴉鶬鴇鴆鴣鶇鸕鴨鴞鴦鴒鴟鴝鴛鴬鴕鷥鷙鴯鴰鵂鴴鵃鴿鸞鴻鵐鵓鸝鵑鵠鵝鵒鷳鵜鵡鵲鶓鵪鶤鵯鵬鵮鶉鶊鵷鷫鶘鶡鶚鶻鶿鶥鶩鷊鷂鶲鶹鶺鷁鶼鶴鷖鸚鷓鷚鷯鷦鷲鷸鷺鸇鷹鸌鸏鸛鸘鹺麥麩黃黌黶黷黲黽黿鼂鼉鞀鼴齇齊齏齒齔齕齗齟齡齙齠齜齦齬齪齲齷龍龔龕龜誌製谘隻裡係範鬆冇嚐嘗鬨麵準鐘彆閒乾儘臟拚";
        public List<string> m_TC_CDATA = new List<string>();
        public SimpleIniData config = new SimpleIniData();
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
        //#DEFAULT
        //#BIG5
        //#PASTE
        public List<string> sendkey_paste_ctrl_v_apps = new List<string>(TextOutputCompatibilityDefaults.PasteCtrlVApps); //使用複製文字貼上出字的 ctrl + v app
        public List<string> sendkey_paste_shift_ins_apps = new List<string>(TextOutputCompatibilityDefaults.PasteShiftInsertApps); //使用複製文字貼上出字的 shift + ins app
        public List<string> sendkey_paste_big5_apps = new List<string>(TextOutputCompatibilityDefaults.PasteBig5Apps); //使用 big5 複製文字貼上出字的 app
        public List<string> sendkey_not_use_ucl_apps = new List<string>(TextOutputCompatibilityDefaults.NoUclApps); //無法使用肥米的 app

        

        public string same_sound_last_word = "";
        public bool is_need_use_pinyi = false;
        public bool is_need_use_phone = false;
        public PhoneCodeTable phone_code_table = PhoneCodeTable.Empty();
        public List<string> ucl_find_data = new List<string>();
        int same_sound_index = 0; //用來放第幾頁
        int same_sound_max_word = 6; //一頁最多5字
        bool is_has_more_page = false; //是否還有下頁
        public bool is_display_sp = false; //是否顯示簡根
        private ShortModeWordLayoutKind currentWordLayoutKind = ShortModeWordLayoutKind.Hint;
        private bool currentWordHasMorePage = false;
        //# GUI Font
        public Font GUI_FONT_12 = new Font("roman", 12, FontStyle.Bold);
        public Font GUI_FONT_14 = new Font("roman", 14, FontStyle.Bold);
        public Font GUI_FONT_16 = new Font("roman", 16, FontStyle.Bold);
        public Font GUI_FONT_18 = new Font("roman", 18, FontStyle.Bold);
        public Font GUI_FONT_20 = new Font("roman", 20, FontStyle.Bold);
        public Font GUI_FONT_22 = new Font("roman", 22, FontStyle.Bold);
        public Font GUI_FONT_26 = new Font("roman", 26, FontStyle.Bold);
        private readonly TimeSpan foregroundProcessCacheDuration = TimeSpan.FromMilliseconds(KeyboardHookPerformancePolicy.ForegroundProcessCacheMilliseconds);
        private DateTime foregroundProcessCachedAt = DateTime.MinValue;
        private IntPtr foregroundProcessCachedHandle = IntPtr.Zero;
        private Dictionary<string, string> foregroundProcessCache = null;
        private DateTime foregroundProcessNameCachedAt = DateTime.MinValue;
        private IntPtr foregroundProcessNameCachedHandle = IntPtr.Zero;
        private string foregroundProcessNameCache = "";
        private bool? isWindows11OrLaterCache = null;
        static Form1 f;
        public uclliu(ref Form1 _f)
        {
            f = _f;
            tsfBridgeManager = new TsfBridgeManager(my.pwd());
            selectedTextTransformDispatcher = new SelectedTextTransformDispatcher(post_ui_action, set_is_send_ucl, debug_print);
            deferredTextOutputDispatcher = new DeferredTextOutputDispatcher(post_ui_action);
            labelUpdateBatcher = new UiLabelUpdateBatcher(post_form_ui_action, apply_label_update_batch);
            performanceLogger = new AsyncPerformanceLogger(my.pwd() + "\\UCLLIU_performance.log");
        }
        //感謝台灣碼農
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
        //感謝台灣碼農
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
        public void generator_sp_table()
        {
            //產生最簡根速查表
            uclcode_r.Clear();
            uclcode_rr.Clear();
            if (uclcode == null)
            {
                return;
            }

            LiuReverseLookupTable reverseLookup = LiuReverseLookupTable.Build(uclcode);
            foreach (KeyValuePair<string, string> pair in reverseLookup.WordToRoot)
            {
                uclcode_r[pair.Key] = pair.Value;
            }
            foreach (KeyValuePair<string, string> pair in reverseLookup.CodeToWord)
            {
                uclcode_rr[pair.Key] = pair.Value;
            }
            //Console.WriteLine("測試簡根:" + uclcode_r["臨"]);
        }
        public void run_about_ucl()
        {
            MessageBox.Show(new Form { TopMost = true },
                    about_uclliu(),
                    "羽山の說明",
                    MessageBoxButtons.OK,
                    MessageBoxIcon.Information);
        }
        public bool run_extra() //跑額外的功能，如 ,,,version
        {
            string code = "";
            code = "';";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                if (start_phone_mode())
                {
                    last_key = "";
                    return true;
                }
            }
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
                run_about_ucl();
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
            code = ",,,z";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code && is_ucl())
            {
                //# 將框選的文字，轉成嘸蝦米的字
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                toAlphaOrNonAlpha();
                last_key = "";
                selectedTextTransformDispatcher.Queue(
                    selectedTextTransformCommand,
                    ",,,z",
                    delegate(string selectedText)
                    {
                        return word_to_sp(simple2trad(selectedText));
                    },
                    delegate(string output)
                    {
                        senddata(output);
                    });
                return true;
            }
            code = ",,,x";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code && is_ucl())
            {
                //# 將框選嘸蝦米的文字，轉成中文字
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                toAlphaOrNonAlpha();
                last_key = "";
                selectedTextTransformDispatcher.Queue(
                    selectedTextTransformCommand,
                    ",,,x",
                    delegate(string selectedText)
                    {
                        return sp_to_word(selectedText);
                    },
                    delegate(string output)
                    {
                        senddata(output);
                    });
                return true;
            }
            code = ",,,box";
            if (last_key.Length >= code.Length && last_key.Substring(last_key.Length - code.Length, code.Length) == code)
            {
                play_ucl_label = "";
                ucl_find_data = new List<string>();
                type_label_set_text();
                toAlphaOrNonAlpha();
                last_key = "";
                f.OpenCustomDictionaryWindow();
                return true;
            }
            return false;
        }
        public void run_toggle_sp()
        {
            is_display_sp = ShortRootDisplaySetting.Toggle(config, is_display_sp, saveConfig);
        }
        private void post_ui_action(Action action)
        {
            if (action == null)
            {
                return;
            }

            try
            {
                if (f != null && !f.IsDisposed && f.IsHandleCreated)
                {
                    f.BeginInvoke(action);
                    return;
                }
            }
            catch (Exception ex)
            {
                debug_print("post ui action fallback: " + ex.Message);
            }

            Thread thread = new Thread(delegate()
            {
                action();
            });
            thread.IsBackground = true;
            thread.SetApartmentState(ApartmentState.STA);
            thread.Start();
        }
        private void set_is_send_ucl(bool value)
        {
            is_send_ucl = value;
        }
        public void queue_senddata(string data)
        {
            deferredTextOutputDispatcher.Queue(data, prepare_senddata_text, send_prepared_output);
        }
        public void queue_senddata_with_labels(string data)
        {
            string labelText = data;
            deferredTextOutputDispatcher.Queue(
                data,
                delegate(string output)
                {
                    string preparedOutput = prepare_senddata_text(output);
                    show_sp_to_label(labelText);
                    show_phone_to_label(labelText);
                    return preparedOutput;
                },
                send_prepared_output);
        }
        public bool start_phone_mode()
        {
            if (phone_code_table == null || !phone_code_table.IsAvailable)
            {
                return false;
            }

            is_need_use_pinyi = false;
            is_need_use_phone = true;
            play_ucl_label = "";
            ucl_find_data = new List<string>();
            same_sound_index = 0;
            is_has_more_page = false;
            type_label_set_text("注:");
            toAlphaOrNonAlpha();
            return true;
        }
        public void stop_phone_mode()
        {
            is_need_use_phone = false;
            if (play_ucl_label.Length == 0)
            {
                type_label_set_text();
            }
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
            play_ucl_label = "";
            ucl_find_data = new List<string>();
            is_need_use_phone = false;
            config["DEFAULT"]["SHORT_MODE"] = "1";
            clear_input_labels_for_mode_change();
            update_UI();
            saveConfig();
        }
        public void run_long()
        {
            //f.word_label.Visible = false;
            //f.type_label.Vset_visible(False)
            play_ucl_label = "";
            ucl_find_data = new List<string>();
            is_need_use_phone = false;
            config["DEFAULT"]["SHORT_MODE"] = "0";
            clear_input_labels_for_mode_change();
            update_UI();
            saveConfig();
        }
        private void clear_input_labels_for_mode_change()
        {
            currentWordLayoutKind = ShortModeWordLayoutKind.Hint;
            currentWordHasMorePage = false;
            outputHintComposer.BeginOutput();
            if (f == null || f.IsDisposed)
            {
                return;
            }

            f.type_label.Text = "";
            f.type_label.ForeColor = Color.Black;
            f.word_label.Text = "";
            f.word_label.ForeColor = Color.Black;
        }
        public string about_uclliu()
        {
            return UclLiuAppInfo.BuildAboutText();
        }
        ///字串轉全形
        /// From : https://dotblogs.com.tw/shunnien/2013/07/21/111737
        ///</summary>
        ///<param name="input">任一字元串</param>
        ///<returns>全形字元串</returns>
        private string ToWide(string input)
        {
            char[] c = input.ToCharArray();
            for (int i = 0; i < c.Length; i++)
            {
                // Full-width space is 12288, half-width space is 32
                if (c[i] == 32)
                {
                    c[i] = (char)12288;
                }
                // For other characters, the half-width (33-126) and full-width (65281-65374) differ by 65248
                else if (c[i] >= 33 && c[i] <= 126)
                {
                    c[i] = (char)(c[i] + 65248);
                }
            }
            return new string(c);
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
                debug_print("肥米已經被開啟了，取消..." + ex.Message);
                return false;
            }
        }
        public void loadJsonData()
        {
            string liu_json_path = my.pwd() + "\\liu.json";
            try
            {
                LiuTableConverter.EnsureLiuJson(my.pwd(), debug_print);
            }
            catch (Exception ex)
            {
                debug_print("字根檔自動轉換失敗：" + ex.Message);
            }
            if (!my.is_file(liu_json_path))
            {
                MessageBox.Show("查無 liu.json 檔...");
                f.btn_X.PerformClick();
            }
            try
            {
                string data = my.b2s(my.file_get_contents(liu_json_path));
                uclcode = LiuJsonTable.ParseChardefsRoot(my.json_decode(data));
                loadCustomDictionaryData();
                if (uclcode.ContainsKey("ucl"))
                {
                    debug_print(my.implode(",", uclcode["ucl"]));
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show("liu.json 檔內容解算錯誤...");
                debug_print("liu.json 檔內容解算錯誤..." + ex.Message);
                f.btn_X.PerformClick();
            }

        }
        public void loadCustomDictionaryData()
        {
            try
            {
                Dictionary<string, List<string>> customEntries = CustomDictionaryStore.Load(CUSTOM_JSON_FILE, debug_print);
                int addedCount = CustomDictionaryStore.MergeInto(uclcode, customEntries);
                if (addedCount > 0)
                {
                    debug_print("自定詞庫載入完成：" + addedCount.ToString());
                }
            }
            catch (Exception ex)
            {
                // 自定詞庫錯誤不應阻斷主字根載入，避免使用者因 custom.json 格式錯誤完全不能打字。
                debug_print("自定詞庫載入失敗：" + ex.Message);
            }
        }
        public void reload_word_root()
        {
            loadJsonData();
            generator_sp_table();
            play_ucl_label = "";
            ucl_find_data = new List<string>();
            type_label_set_text();
        }
        public void saveConfig()
        {
            SimpleIniFile.WriteFile(INI_CONFIG_FILE, config);
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
            int screen_width = Screen.PrimaryScreen.Bounds.Width;
            int screen_height = Screen.PrimaryScreen.Bounds.Height;
            config["DEFAULT"]["X"] = (screen_width - 700).ToString();
            config["DEFAULT"]["Y"] = (screen_height * 0.87).ToString();
            config["DEFAULT"]["ALPHA"] = "1"; //#嘸蝦米全顯示時時的初值
            config["DEFAULT"]["SHORT_MODE"] = "0"; // #0:簡短畫面，或1:長畫面
            config["DEFAULT"]["ZOOM"] = "1"; //#整體比例大小
            config["DEFAULT"]["SEND_KIND_1_PASTE"] = ""; //#出字模式1
            config["DEFAULT"]["SEND_KIND_2_BIG5"] = ""; //#出字模式2
            config["DEFAULT"]["SEND_KIND_3_NOUCL"] = ""; //#Force no UCL
            config["DEFAULT"]["KEYBOARD_VOLUME"] = "30";
            config["DEFAULT"]["SP"] = "0"; //#是否顯示簡根
            config["DEFAULT"]["SHOW_PHONE_CODE"] = "0"; //#顯示注音讀音
            config["DEFAULT"]["CTRL_SP"] = "0"; //#使用CTRL+SPACE換肥米
            config["DEFAULT"]["PLAY_SOUND_ENABLE"] = "0"; //#打字音
            config["DEFAULT"]["STARTUP_DEFAULT_UCL"] = "1"; //#啟動時，預設為 肥，改為 0 則為 英
            config["DEFAULT"]["ENABLE_HALF_FULL"] = "1"; //#允許切換 全形半形
            config["DEFAULT"]["TSF_BRIDGE_TIMEOUT_MS"] = TsfBridgeConstants.DefaultTimeoutMilliseconds.ToString(); //#TSF Bridge pipe timeout

            debug_print(INI_CONFIG_FILE);
            if (my.is_file(INI_CONFIG_FILE))
            {
                //轉回全大寫放回
                string data = my.b2s(my.file_get_contents(INI_CONFIG_FILE));
                data = data.ToUpper();
                my.file_put_contents(INI_CONFIG_FILE, data);
                try
                {
                    SimpleIniData _config = SimpleIniFile.ReadFile(INI_CONFIG_FILE);

                    foreach (KeyValuePair<string, string> key in _config["DEFAULT"].Keys)
                    {
                        config["DEFAULT"][key.Key] = key.Value.Trim();
                        //debug_print(key.Key);
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
            config["DEFAULT"]["SEND_KIND_1_PASTE"] = config["DEFAULT"]["SEND_KIND_1_PASTE"].ToString().Trim();
            config["DEFAULT"]["SEND_KIND_2_BIG5"] = config["DEFAULT"]["SEND_KIND_2_BIG5"].ToString().Trim();
            config["DEFAULT"]["KEYBOARD_VOLUME"] = TypingSoundVolume.Normalize(config["DEFAULT"]["KEYBOARD_VOLUME"], 30).ToString();
            config["DEFAULT"]["SP"] = ShortRootDisplaySetting.Normalize(config["DEFAULT"]["SP"]);
            is_display_sp = ShortRootDisplaySetting.IsEnabled(config["DEFAULT"]["SP"]);
            config["DEFAULT"]["SHOW_PHONE_CODE"] = Convert.ToInt32(config["DEFAULT"]["SHOW_PHONE_CODE"]).ToString();
            if (Convert.ToInt32(config["DEFAULT"]["SHOW_PHONE_CODE"]) <= 0)
            {
                config["DEFAULT"]["SHOW_PHONE_CODE"] = "0";
            }
            else
            {
                config["DEFAULT"]["SHOW_PHONE_CODE"] = "1";
            }
            config["DEFAULT"]["CTRL_SP"] = Convert.ToInt32(config["DEFAULT"]["CTRL_SP"]).ToString();
            if (Convert.ToInt32(config["DEFAULT"]["CTRL_SP"]) <= 0)
            {
                config["DEFAULT"]["CTRL_SP"] = "0";
            }
            else
            {
                config["DEFAULT"]["CTRL_SP"] = "1";
            }
            config["DEFAULT"]["PLAY_SOUND_ENABLE"] = Convert.ToInt32(config["DEFAULT"]["PLAY_SOUND_ENABLE"]).ToString();
            if (Convert.ToInt32(config["DEFAULT"]["PLAY_SOUND_ENABLE"]) <= 0)
            {
                config["DEFAULT"]["PLAY_SOUND_ENABLE"] = "0";
            }
            else
            {
                config["DEFAULT"]["PLAY_SOUND_ENABLE"] = "1";
            }
            config["DEFAULT"]["STARTUP_DEFAULT_UCL"] = Convert.ToInt32(config["DEFAULT"]["STARTUP_DEFAULT_UCL"]).ToString();
            if (Convert.ToInt32(config["DEFAULT"]["STARTUP_DEFAULT_UCL"]) <= 0)
            {
                config["DEFAULT"]["STARTUP_DEFAULT_UCL"] = "0";
            }
            else
            {
                config["DEFAULT"]["STARTUP_DEFAULT_UCL"] = "1";
            }
            config["DEFAULT"]["ENABLE_HALF_FULL"] = Convert.ToInt32(config["DEFAULT"]["ENABLE_HALF_FULL"]).ToString();
            if (Convert.ToInt32(config["DEFAULT"]["ENABLE_HALF_FULL"]) <= 0)
            {
                config["DEFAULT"]["ENABLE_HALF_FULL"] = "0";
            }
            else
            {
                config["DEFAULT"]["ENABLE_HALF_FULL"] = "1";
            }
            config["DEFAULT"]["TSF_BRIDGE_TIMEOUT_MS"] = TsfBridgeSettings.NormalizeTimeout(config["DEFAULT"]["TSF_BRIDGE_TIMEOUT_MS"], TsfBridgeConstants.DefaultTimeoutMilliseconds).ToString();
            config["DEFAULT"]["SEND_KIND_1_PASTE"] = config["DEFAULT"]["SEND_KIND_1_PASTE"].Trim();
            config["DEFAULT"]["SEND_KIND_1_PASTE"] = config["DEFAULT"]["SEND_KIND_1_PASTE"].Replace("\"", "");
            config["DEFAULT"]["SEND_KIND_2_BIG5"] = config["DEFAULT"]["SEND_KIND_2_BIG5"].Trim();
            config["DEFAULT"]["SEND_KIND_2_BIG5"] = config["DEFAULT"]["SEND_KIND_2_BIG5"].Replace("\"", "");
            config["DEFAULT"]["SEND_KIND_3_NOUCL"] = config["DEFAULT"]["SEND_KIND_3_NOUCL"].Replace("\"", "");
            add_configured_apps(sendkey_paste_shift_ins_apps, config["DEFAULT"]["SEND_KIND_1_PASTE"]);
            add_configured_apps(sendkey_paste_big5_apps, config["DEFAULT"]["SEND_KIND_2_BIG5"]);
            add_configured_apps(sendkey_not_use_ucl_apps, config["DEFAULT"]["SEND_KIND_3_NOUCL"]);

            clear_input_labels_for_mode_change();
            update_UI();
            //不管如何，先存一次
            saveConfig();
        }
        private void add_configured_apps(List<string> appList, string configValue)
        {
            if (appList == null || string.IsNullOrWhiteSpace(configValue))
            {
                return;
            }

            string[] apps = my.explode(",", configValue);
            for (int i = 0; i < apps.Length; i++)
            {
                string app = apps[i].Trim();
                if (app.Length == 0)
                {
                    continue;
                }
                if (!TextOutputRouter.MatchesProcess(app, appList))
                {
                    appList.Add(app);
                }
            }
        }
        public int get_keyboard_volume()
        {
            return TypingSoundVolume.Normalize(config["DEFAULT"]["KEYBOARD_VOLUME"], 30);
        }
        public void handle_typing_sound(bool keydown, bool keyup, int keyCode)
        {
            if (keyup)
            {
                typingSoundPlayer.HandleKey(false, true, keyCode, get_keyboard_volume());
                return;
            }

            if (config["DEFAULT"]["PLAY_SOUND_ENABLE"] != "1")
            {
                return;
            }

            typingSoundPlayer.HandleKey(keydown, false, keyCode, get_keyboard_volume());
        }
        public void preview_typing_sound()
        {
            typingSoundPlayer.PreviewForKey(0, get_keyboard_volume());
        }
        public void reload_typing_sound()
        {
            typingSoundPlayer.Reload();
        }
        public void preload_typing_sound()
        {
            if (config["DEFAULT"]["PLAY_SOUND_ENABLE"] == "1")
            {
                typingSoundPlayer.WarmUp(get_keyboard_volume());
            }
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
            bool isShortMode = config["DEFAULT"]["SHORT_MODE"] == "1";
            double zoom = Convert.ToDouble(config["DEFAULT"]["ZOOM"]);
            int chromeButtonWidth = Convert.ToInt32(40 * zoom);

            //Control c_type_label = f.LP.GetControlFromPosition(3, 1);
            f.LP.CellBorderStyle = isShortMode
                ? System.Windows.Forms.TableLayoutPanelCellBorderStyle.None
                : System.Windows.Forms.TableLayoutPanelCellBorderStyle.Inset;
            f.LP.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.LP.AutoSize = true;
            f.LP.Width = 10;
            f.LP.Height = 10;
            f.LP.RowStyles[0] = new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, chromeButtonWidth);
            //btn_UCL
            set_column_width(0, chromeButtonWidth);
            //btn_HALF
            set_column_width(1, chromeButtonWidth);

            //btn_gamemode
            if (isShortMode)
            {
                //短
                f.btn_gamemode.Visible = false;
                apply_short_mode_columns(currentWordLayoutKind, currentWordHasMorePage);

            }
            else
            {
                //tape_label
                TableLayoutModeTransition.RestoreLongModeColumns(
                    f.LP,
                    f.type_label,
                    f.word_label,
                    f.btn_simple,
                    f.btn_gamemode,
                    f.btn_X);
                f.type_label.Visible = true;
                f.word_label.Visible = true;
                f.btn_gamemode.Visible = true;
                set_column_width(2, Convert.ToInt32(150 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
                //word_label
                set_column_width(3, Convert.ToInt32(350 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
                //btn_gamemode
                set_column_width(5, Convert.ToInt32(120 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));

                //殘/正
                if (is_simple())
                {
                    //殘模式
                    set_column_width(4, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
                }
                else
                {
                    //正模式
                    set_column_width(4, 0);
                }


                //btn_X
                set_column_width(6, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
            }

            // 肥
            f.btn_UCL.Font = GUI_FONT_16;
            int controlBorderSize = isShortMode ? 1 : 0;
            BorderStyle labelBorderStyle = isShortMode ? BorderStyle.FixedSingle : BorderStyle.None;
            Padding mainChromePadding = isShortMode
                ? ChromeButtonTextAlignment.ShortModePadding(ChromeButtonTextKind.MainCjk, Convert.ToDouble(config["DEFAULT"]["ZOOM"]))
                : ChromeButtonTextAlignment.LongModePadding();
            Padding closeChromePadding = isShortMode
                ? ChromeButtonTextAlignment.ShortModePadding(ChromeButtonTextKind.Close, Convert.ToDouble(config["DEFAULT"]["ZOOM"]))
                : ChromeButtonTextAlignment.LongModePadding();

            f.btn_UCL.FlatAppearance.BorderSize = controlBorderSize;
            f.btn_UCL.Margin = new System.Windows.Forms.Padding(0);
            f.btn_UCL.Padding = mainChromePadding;
            f.btn_UCL.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_UCL.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;
            f.btn_UCL.MinimumSize = isShortMode ? new Size(chromeButtonWidth, chromeButtonWidth) : Size.Empty;

            // 半全
            f.btn_HALF.Font = GUI_FONT_16;
            f.btn_HALF.FlatAppearance.BorderSize = controlBorderSize;
            f.btn_HALF.Margin = new System.Windows.Forms.Padding(0);
            f.btn_HALF.Padding = mainChromePadding;
            f.btn_HALF.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_HALF.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;
            f.btn_HALF.MinimumSize = isShortMode ? new Size(chromeButtonWidth, chromeButtonWidth) : Size.Empty;

            // 輸五
            f.type_label.Font = GUI_FONT_18;

            f.type_label.Margin = new System.Windows.Forms.Padding(0);
            f.type_label.Padding = new System.Windows.Forms.Padding(0);
            f.type_label.BorderStyle = labelBorderStyle;
            f.type_label.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.type_label.TextAlign = isShortMode ? ContentAlignment.MiddleCenter : ContentAlignment.MiddleLeft;
            f.type_label.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 字
            f.word_label.Font = GUI_FONT_18;
            f.word_label.Margin = new System.Windows.Forms.Padding(0);
            f.word_label.Padding = new System.Windows.Forms.Padding(0);
            f.word_label.BorderStyle = labelBorderStyle;
            f.word_label.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.word_label.TextAlign = isShortMode ? ContentAlignment.MiddleCenter : ContentAlignment.MiddleLeft;
            f.word_label.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 簡
            f.btn_simple.Font = isShortMode ? GUI_FONT_16 : GUI_FONT_18;
            f.btn_simple.Margin = new System.Windows.Forms.Padding(0);
            f.btn_simple.Padding = new System.Windows.Forms.Padding(0);
            f.btn_simple.FlatAppearance.BorderSize = controlBorderSize;
            f.btn_simple.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_simple.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // 遊戲
            f.btn_gamemode.Font = GUI_FONT_14;
            f.btn_gamemode.Margin = new System.Windows.Forms.Padding(0);
            f.btn_gamemode.Padding = new System.Windows.Forms.Padding(0);
            f.btn_gamemode.FlatAppearance.BorderSize = controlBorderSize;
            f.btn_gamemode.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_gamemode.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;

            // X
            f.btn_X.Font = GUI_FONT_16;
            f.btn_X.Margin = new System.Windows.Forms.Padding(0);
            f.btn_X.Padding = closeChromePadding;
            f.btn_X.FlatAppearance.BorderSize = controlBorderSize;
            f.btn_X.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            f.btn_X.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;
            f.btn_X.MinimumSize = isShortMode ? new Size(chromeButtonWidth, chromeButtonWidth) : Size.Empty;
            f.btn_UCL.TextAlign = ContentAlignment.MiddleCenter;
            f.btn_HALF.TextAlign = ContentAlignment.MiddleCenter;
            f.btn_simple.TextAlign = ContentAlignment.MiddleCenter;
            f.btn_gamemode.TextAlign = ContentAlignment.MiddleCenter;
            f.btn_X.TextAlign = ContentAlignment.MiddleCenter;

            f.LP.Anchor = AnchorStyles.Left | AnchorStyles.Top | AnchorStyles.Right | AnchorStyles.Bottom;
            f.LP.Dock = DockStyle.Fill;
            //f.LP.MaximumSize = new Size(200, 50);
            f.LP.AutoSize = true;
            f.LP.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_UCL.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_HALF.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_gamemode.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.btn_X.AutoSizeMode = AutoSizeMode.GrowAndShrink;
            f.type_label.AutoSize = false;
            f.word_label.AutoSize = false;
            f.Visible = true;
            //f.Refresh();
            f.ResumeLayout();
        }
        private void update_short_mode_columns()
        {
            if (config["DEFAULT"]["SHORT_MODE"] != "1")
            {
                return;
            }

            apply_short_mode_columns(currentWordLayoutKind, currentWordHasMorePage);
        }
        private void apply_short_mode_columns(ShortModeWordLayoutKind wordLayoutKind, bool wordHasMorePage)
        {
            double zoom = Convert.ToDouble(config["DEFAULT"]["ZOOM"]);
            int maxWidth = get_short_mode_max_width();
            ShortModeLabelLayout typeLayout = UiLayoutCalculator.ShortModeMeasuredTypeLayout(
                f.type_label.Text,
                measure_short_mode_text_width(f.type_label.Text, GUI_FONT_18),
                zoom,
                ShortModeTextPadding,
                maxWidth);
            ShortModeLabelLayout wordLayout = UiLayoutCalculator.ShortModeMeasuredWordLayout(
                f.word_label.Text,
                measure_short_mode_text_width(f.word_label.Text, GUI_FONT_18),
                zoom,
                ShortModeTextPadding,
                wordLayoutKind,
                wordHasMorePage,
                maxWidth);
            int buttonWidth = Convert.ToInt32(40 * zoom);
            ShortModePackedLayoutPlan plan = UiLayoutCalculator.BuildShortModePackedLayout(
                typeLayout,
                wordLayout,
                is_simple(),
                buttonWidth,
                f.LP.ColumnStyles.Count);
            if (!has_short_mode_packed_layout_change(plan, typeLayout, wordLayout))
            {
                return;
            }

            f.LP.SuspendLayout();
            try
            {
                if (f.type_label.Visible != typeLayout.Visible)
                {
                    f.type_label.Visible = typeLayout.Visible;
                }
                if (f.word_label.Visible != wordLayout.Visible)
                {
                    f.word_label.Visible = wordLayout.Visible;
                }
                if (f.btn_gamemode.Visible)
                {
                    f.btn_gamemode.Visible = false;
                }
                set_control_column_span(f.type_label, plan.TypeColumn, plan.TypeColumnSpan);
                set_control_column_span(f.word_label, plan.WordColumn, plan.WordColumnSpan);
                set_control_column_span(f.btn_simple, plan.SimpleColumn, plan.SimpleColumnSpan);
                set_control_column_span(f.btn_X, plan.CloseColumn, plan.CloseColumnSpan);

                for (int i = 2; i < plan.ColumnWidths.Length && i < f.LP.ColumnStyles.Count; i++)
                {
                    set_column_width(i, plan.ColumnWidths[i]);
                }
            }
            finally
            {
                f.LP.ResumeLayout(true);
            }
        }
        private bool has_short_mode_packed_layout_change(ShortModePackedLayoutPlan plan, ShortModeLabelLayout typeLayout, ShortModeLabelLayout wordLayout)
        {
            if (f.type_label.Visible != typeLayout.Visible || f.word_label.Visible != wordLayout.Visible || f.btn_gamemode.Visible)
            {
                return true;
            }

            for (int i = 2; i < plan.ColumnWidths.Length && i < f.LP.ColumnStyles.Count; i++)
            {
                if (get_column_width(i) != plan.ColumnWidths[i])
                {
                    return true;
                }
            }

            return has_control_column_span_change(f.type_label, plan.TypeColumn, plan.TypeColumnSpan)
                || has_control_column_span_change(f.word_label, plan.WordColumn, plan.WordColumnSpan)
                || has_control_column_span_change(f.btn_simple, plan.SimpleColumn, plan.SimpleColumnSpan)
                || has_control_column_span_change(f.btn_X, plan.CloseColumn, plan.CloseColumnSpan);
        }
        private ShortModeColumnState get_current_short_mode_column_state()
        {
            return new ShortModeColumnState(
                get_column_width(2),
                f.type_label.Visible,
                get_column_width(3),
                f.word_label.Visible);
        }
        private int get_column_width(int index)
        {
            if (index < 0 || index >= f.LP.ColumnStyles.Count)
            {
                return 0;
            }

            return Convert.ToInt32(f.LP.ColumnStyles[index].Width);
        }
        private bool has_control_column_span_change(Control control, int column, int span)
        {
            return f.LP.GetColumn(control) != column || f.LP.GetColumnSpan(control) != span;
        }
        private void set_control_column_span(Control control, int column, int span)
        {
            int columnCount = f.LP.ColumnStyles.Count;
            if (columnCount <= 0)
            {
                return;
            }
            if (column < 0)
            {
                column = 0;
            }
            if (column >= columnCount)
            {
                column = columnCount - 1;
            }
            if (span < 1)
            {
                span = 1;
            }
            if (column + span > columnCount)
            {
                span = columnCount - column;
            }
            if (f.LP.GetColumn(control) != column)
            {
                f.LP.SetColumn(control, column);
            }
            if (f.LP.GetColumnSpan(control) != span)
            {
                f.LP.SetColumnSpan(control, span);
            }
        }
        private void queue_type_label_update(string text, Color foreColor)
        {
            labelUpdateBatcher.QueueType(text, foreColor);
        }
        private void queue_word_label_update(string text)
        {
            queue_word_label_update(text, null, ShortModeWordLayoutKind.Hint, false);
        }
        private void queue_word_label_update(string text, Color? foreColor)
        {
            queue_word_label_update(text, foreColor, ShortModeWordLayoutKind.Hint, false);
        }
        private void queue_word_label_update(string text, Color? foreColor, ShortModeWordLayoutKind layoutKind, bool hasMorePage)
        {
            labelUpdateBatcher.QueueWord(text, foreColor, layoutKind, hasMorePage);
        }
        private void apply_label_update_batch(UiLabelUpdateSnapshot snapshot)
        {
            if (snapshot == null)
            {
                return;
            }

            if (snapshot.UpdateType)
            {
                f.type_label.Text = snapshot.TypeText;
                f.type_label.ForeColor = snapshot.TypeColor;
            }
            if (snapshot.UpdateWord)
            {
                f.word_label.Text = snapshot.WordText;
                if (snapshot.WordHasColor)
                {
                    f.word_label.ForeColor = snapshot.WordColor;
                }
                currentWordLayoutKind = snapshot.WordLayoutKind;
                currentWordHasMorePage = snapshot.WordHasMorePage;
            }

            update_short_mode_columns();
        }
        private void post_form_ui_action(Action action)
        {
            if (action == null || f == null || f.IsDisposed)
            {
                return;
            }

            MethodInvoker invoker = delegate
            {
                try
                {
                    if (f != null && !f.IsDisposed)
                    {
                        action();
                    }
                }
                catch (Exception ex)
                {
                    debug_print("post form ui action failed: " + ex.Message);
                }
            };

            try
            {
                if (f.IsHandleCreated)
                {
                    f.BeginInvoke(invoker);
                    return;
                }
            }
            catch (Exception ex)
            {
                debug_print("begin invoke label update failed: " + ex.Message);
            }

            invoker();
        }
        private int get_short_mode_max_width()
        {
            return Math.Max(0, Screen.PrimaryScreen.WorkingArea.Width - 160);
        }
        private int measure_short_mode_text_width(string text, Font font)
        {
            if (String.IsNullOrEmpty(text))
            {
                return 0;
            }
            Size size = TextRenderer.MeasureText(
                text,
                font,
                Size.Empty,
                TextFormatFlags.NoPadding | TextFormatFlags.SingleLine);
            return size.Width;
        }
        private void set_column_width(int index, int width)
        {
            if (index < 0 || index >= f.LP.ColumnStyles.Count)
            {
                return;
            }

            System.Windows.Forms.ColumnStyle style = f.LP.ColumnStyles[index];
            if (style.SizeType != System.Windows.Forms.SizeType.Absolute)
            {
                style.SizeType = System.Windows.Forms.SizeType.Absolute;
            }
            if (Math.Abs(style.Width - width) > 0.5)
            {
                style.Width = width;
            }
        }
        public void show_sp_to_label(string data)
        {
            show_sp_to_label(data, false);
        }
        public void show_sp_to_label(string data, bool force)
        {
            //# 顯示最簡字根到輸入結束框後

            if (is_display_sp == false && !force)
            {
                return;
            }
            string sp = "簡根：" + my.strtoupper(word_to_sp(data));
            //#word_label.set_label(sp)
            //#word_label.modify_font(pango.FontDescription(GUI_FONT_18))
            type_label_set_text(outputHintComposer.SetShortRootHint(sp));
        }
        public void show_phone_to_label(string data)
        {
            show_phone_to_label(data, false);
        }
        public void show_phone_to_label(string data, bool force)
        {
            if (!force && config["DEFAULT"]["SHOW_PHONE_CODE"] == "0")
            {
                return;
            }
            if (phone_code_table == null || !phone_code_table.IsAvailable)
            {
                return;
            }

            List<string> phones = phone_code_table.GetPhonesForWord(data);
            if (phones.Count == 0)
            {
                return;
            }

            string readPhone = String.Join("或", phones.ToArray());
            type_label_set_text(outputHintComposer.SetPhoneHint(readPhone));
        }
        public string find_ucl_in_uclcode(string chinese_data)
        {
            //# 用中文反找蝦碼(V1.10版寫法)
            //一次傳一個字
            string root;
            if (uclcode_r.TryGetValue(chinese_data, out root))
            {
                return root;
            }
            else
            {
                return chinese_data;
            }
        }
        public string sp_to_word(string data)
        {
            //字根轉中文
            string selectData = my.trim(data);
            List<string> menter = new List<string>(my.explode("\n", selectData));
            string output = "";
            for (int i = 0, max_i = menter.Count; i < max_i; i++)
            {
                List<string> output_arr = new List<string>();
                List<string> m = new List<string>(my.explode(" ", menter[i]));
                //#print(len(m));
                for (int j = 0, max_j = m.Count; j < max_j; j++)
                {
                    //# 轉小寫
                    string ucl_split_code = my.strtolower(m[j]);
                    output += uclcode_to_chinese(ucl_split_code);
                }
                if (i != menter.Count - 1)
                {
                    output += "\n";
                }
            }
            return output;
        }
        public string word_to_sp(string data)
        {
            //# 中文轉最簡字根
            //# 回傳字根文字
            //# 中文轉字根 thread
            string selectData = data; //#my.trim(data);
            selectData = selectData.Replace("\r", "");
            List<string> menter = new List<string>(my.explode("\n", selectData));
            string output = "";
            for (int i = 0, max_i = menter.Count; i < max_i; i++)
            {
                List<string> output_arr = new List<string>();
                for (int j = 0, max_j = menter[i].Length; j < max_j; j++)
                {
                    string _uclcode = find_ucl_in_uclcode(menter[i][j].ToString());
                    if (_uclcode != "")
                    {
                        output_arr.Add(_uclcode);
                    }
                }
                output += my.implode(" ", output_arr);
                if (i != menter.Count - 1)
                {
                    output += "\n";
                }
            }
            return output;
        }
        public void use_pinyi(string data)
        {
            List<string> candidates = PinyiCandidateSelector.FindCandidates(same_sound_data, data);
            debug_print("Debug Finds: " + candidates.Count.ToString());
            debug_print("Debug same_sound_index: " + same_sound_index.ToString());
            debug_print("Debug same_sound_max_word: " + same_sound_max_word.ToString());
            int maxword = same_sound_index + same_sound_max_word;
            debug_print("Debug maxword: " + maxword.ToString());
            int nextIndex;
            ucl_find_data = PinyiCandidateSelector.PageCandidates(candidates, same_sound_index, same_sound_max_word, out is_has_more_page, out nextIndex);
            debug_print("DEBUG same_sound_index: " + same_sound_index.ToString());
            same_sound_index = nextIndex;
            word_label_set_text();
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
            if (is_need_use_phone)
            {
                handle_phone_key(thekey);
                return;
            }

            play_ucl_label = f.type_label.Text;
            //# 不可以超過5個字
            if (play_ucl_label.Length < 5)
            {
                play_ucl_label = string.Format("{0}{1}", play_ucl_label, thekey);
                type_label_set_text();
            }

        }
        public bool handle_phone_key(string thekey)
        {
            if (!is_need_use_phone || phone_code_table == null || !phone_code_table.IsAvailable || String.IsNullOrEmpty(thekey))
            {
                return false;
            }

            char key = Char.ToLowerInvariant(thekey[0]);
            if (Char.IsDigit(key) && ucl_find_data.Count > 0)
            {
                int index = Convert.ToInt32(key.ToString());
                if (index < ucl_find_data.Count)
                {
                    string data = ucl_find_data[index];
                    is_need_use_phone = false;
                    senddata(data);
                    show_sp_to_label(data, true);
                    show_phone_to_label(data);
                    return true;
                }
            }

            if (key == ' ')
            {
                type_label_set_text("", false);
                return true;
            }

            string phone;
            if (!phone_code_table.TryKeyboardKeyToPhone(key, out phone))
            {
                return false;
            }

            if (play_ucl_label.Length >= 4)
            {
                return true;
            }

            if (play_ucl_label.Length > 0 && is_phone_tone(play_ucl_label[play_ucl_label.Length - 1].ToString()))
            {
                return true;
            }

            play_ucl_label = play_ucl_label + phone;
            type_label_set_text("", !is_phone_tone(phone));
            return true;
        }
        private bool is_phone_tone(string phone)
        {
            return phone == "ˊ" || phone == "ˇ" || phone == "ˋ" || phone == "˙";
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
                        is_need_use_phone = false;
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
                debug_print("Crash..." + ex.Message);
            }
        }
        public void debug_print(string data)
        {
            if (is_DEBUG_mode)
            {
                Console.WriteLine(data);
            }
        }
        public void apply_runtime_performance_tuning()
        {
            RuntimePriorityTuning.ApplyBestEffort(debug_print);
        }
        public void report_keyboard_hook_latency(long startedTicks, int message, int vkCode)
        {
            int elapsedMilliseconds;
            if (!keyboardHookLatencyMonitor.ShouldLog(startedTicks, out elapsedMilliseconds))
            {
                return;
            }

            string logMessage = "slow keyboard hook "
                + elapsedMilliseconds.ToString() + "ms"
                + " message=" + message.ToString()
                + " vk=" + vkCode.ToString()
                + " ucl=" + flag_is_ucl.ToString()
                + " hf=" + flag_is_hf.ToString()
                + " label=" + play_ucl_label
                + " process=" + foregroundProcessNameCache;
            performanceLogger.Log(logMessage);
            debug_print("PERF " + logMessage);
        }
        public bool type_label_set_text(string last_word_label_txt = "")
        {
            return type_label_set_text(last_word_label_txt, false);
        }
        public bool type_label_set_text(string last_word_label_txt, bool showOnly)
        {
            Color normalColor = Color.Black;
            Color hintColor = Color.FromArgb(0, 127, 255);
            queue_type_label_update(play_ucl_label, is_need_use_phone ? hintColor : normalColor);
            if (play_ucl_label.Length > 0)
            {
                debug_print("ShowSearch");
                if (is_need_use_phone)
                {
                    if (!showOnly)
                    {
                        show_search("phone");
                    }
                    else
                    {
                        queue_word_label_update("注:", hintColor);
                    }
                }
                else
                {
                    show_search();
                }
            }
            else
            {
                if (is_need_use_phone)
                {
                    queue_word_label_update("注:", hintColor);
                }
                else
                {
                    queue_word_label_update("", normalColor);
                }
            }
            // 如果 last_word_label_txt 不是空值，代表有簡根或其他用字
            //word_label.modify_fg(gtk.STATE_NORMAL, gtk.gdk.color_parse('black'))
            if (last_word_label_txt != "")
            {
                queue_word_label_update(last_word_label_txt, hintColor);
                //word_label.modify_fg(gtk.STATE_NORMAL, gtk.gdk.Color("#007fff"));
            }
            return true;
        }
        private bool has_ucl_root(string c)
        {
            return uclcode != null && c != null && uclcode.ContainsKey(c);
        }
        private bool try_get_root_candidates(string c, out List<string> candidates)
        {
            candidates = null;
            if (uclcode == null || c == null)
            {
                return false;
            }
            return uclcode.TryGetValue(c, out candidates);
        }
        private bool try_get_suffix_candidate(string c, string suffix, int candidateIndex, out string candidate)
        {
            candidate = "";
            if (c == null || c.Length <= suffix.Length || has_ucl_root(c) || !c.EndsWith(suffix, StringComparison.Ordinal))
            {
                return false;
            }

            string root = c.Substring(0, c.Length - suffix.Length);
            List<string> candidates;
            if (try_get_root_candidates(root, out candidates) && candidates.Count > candidateIndex)
            {
                candidate = candidates[candidateIndex];
                return true;
            }
            return false;
        }
        private bool try_get_suffix_search_candidates(string c, string suffix, int candidateIndex, out List<string> candidates)
        {
            candidates = null;
            string candidate;
            if (!try_get_suffix_candidate(c, suffix, candidateIndex, out candidate))
            {
                return false;
            }

            candidates = new List<string>();
            candidates.Add(candidate);
            return true;
        }
        public string uclcode_to_chinese(string c)
        {
            c = my.strtolower(my.trim(c));
            if (c == "")
            {
                return "";
            }

            string directCandidate;
            if (uclcode_rr.TryGetValue(c, out directCandidate))
            {
                return directCandidate;
            }

            string candidate;
            if (try_get_suffix_candidate(c, "v", 1, out candidate))
            {
                return candidate;
            }
            if (try_get_suffix_candidate(c, "r", 2, out candidate))
            {
                return candidate;
            }
            if (try_get_suffix_candidate(c, "s", 3, out candidate))
            {
                return candidate;
            }
            if (try_get_suffix_candidate(c, "f", 4, out candidate))
            {
                return candidate;
            }
            if (try_get_suffix_candidate(c, "w", 5, out candidate))
            {
                return candidate;
            }

            List<string> candidates;
            if (try_get_root_candidates(c, out candidates) && candidates.Count > 0)
            {
                return candidates[0];
            }

            return c;
        }
        public bool show_search()
        {
            return show_search(null);
        }
        public bool show_search(string kind)
        {
            //#真的要顯示了
            same_sound_index = 0;
            is_has_more_page = false;
            same_sound_last_word = "";
            debug_print("ShowSearch1");
            if (kind == "phone")
            {
                List<string> phoneCandidates = phone_code_table.FindCandidatesByPhoneCode(play_ucl_label);
                int nextIndex;
                ucl_find_data = PinyiCandidateSelector.PageCandidates(phoneCandidates, same_sound_index, same_sound_max_word, out is_has_more_page, out nextIndex);
                same_sound_index = nextIndex;
                word_label_set_text();
                return phoneCandidates.Count > 0;
            }

            string c = play_ucl_label.ToLower().Trim();
            is_need_use_pinyi = false;
            if (c.Length > 1 && c.Substring(0, 1) == "'")
            {
                c = c.Substring(1);
                is_need_use_pinyi = true;
            }
            List<string> candidates;
            if (try_get_suffix_search_candidates(c, "v", 1, out candidates))
            {
                //#print("Debug V1")
                ucl_find_data = candidates;
                word_label_set_text();
                return true;
            }
            else if (try_get_suffix_search_candidates(c, "r", 2, out candidates))
            {
                //#print("Debug V1")
                ucl_find_data = candidates;
                word_label_set_text();
                return true;
            }
            else if (try_get_suffix_search_candidates(c, "s", 3, out candidates))
            {
                //#print("Debug V1")
                ucl_find_data = candidates;
                word_label_set_text();
                return true;
            }
            else if (try_get_suffix_search_candidates(c, "f", 4, out candidates))
            {
                //#print("Debug V1")
                ucl_find_data = candidates;
                word_label_set_text();
                return true;
            }
            else if (try_get_suffix_search_candidates(c, "w", 5, out candidates))
            {
                //#print("Debug V1")
                ucl_find_data = candidates;
                word_label_set_text();
                return true;
            }
            else if (try_get_root_candidates(c, out candidates))
            {
                //# print("Debug V2")
                ucl_find_data = new List<string>(candidates);
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
                if (is_need_use_phone)
                {
                    queue_word_label_update("注:", Color.FromArgb(0, 127, 255));
                }
                else
                {
                    queue_word_label_update("");
                }
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
                queue_word_label_update(tmp, is_need_use_phone ? Color.FromArgb(0, 127, 255) : Color.Black, ShortModeWordLayoutKind.Candidates, is_has_more_page);
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
            DateTime now = DateTime.UtcNow;
            if (foregroundProcessCache != null && handle == foregroundProcessCachedHandle && (now - foregroundProcessCachedAt) <= foregroundProcessCacheDuration)
            {
                return foregroundProcessCache;
            }

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
            string Proc_NAME = "";
            try
            {
                Process p = Process.GetProcessById((int)Proc_PID);
                Proc_NAME = p.ProcessName;
            }
            catch (Exception ex)
            {

                debug_print("Get ProcName failure:" + ex.Message);
            }
            Dictionary<string, string> output = ForegroundProcessSnapshot.Create(Proc_TITLE, Proc_NAME, Proc_PID);
            foregroundProcessCachedHandle = handle;
            foregroundProcessCachedAt = now;
            foregroundProcessCache = output;
            return output;
        }

        public string getForegroundWindowProcessName()
        {
            IntPtr handle = Form1.GetForegroundWindow();
            DateTime now = DateTime.UtcNow;
            if (handle == foregroundProcessNameCachedHandle && (now - foregroundProcessNameCachedAt) <= foregroundProcessCacheDuration)
            {
                return foregroundProcessNameCache;
            }

            uint Proc_PID;
            Form1.GetWindowThreadProcessId(handle, out Proc_PID);
            string processName = "";
            try
            {
                Process p = Process.GetProcessById((int)Proc_PID);
                processName = p.ProcessName;
            }
            catch (Exception ex)
            {
                debug_print("Get foreground process name failure:" + ex.Message);
            }

            foregroundProcessNameCachedHandle = handle;
            foregroundProcessNameCachedAt = now;
            foregroundProcessNameCache = ForegroundProcessSnapshot.NormalizeProcessName(processName);
            return foregroundProcessNameCache;
        }

        private bool try_send_output(TextOutputMode outputMode, string data, int foregroundProcessId, out string error)
        {
            error = null;
            is_send_ucl = true;
            try
            {
                switch (outputMode)
                {
                    case TextOutputMode.UnicodeSendInput:
                        return unicodeSendInputOutput.TrySendText(data, out error);
                    case TextOutputMode.WindowMessageChar:
                        return windowMessageCharOutput.TrySendText(data, out error);
                    case TextOutputMode.PasteShiftInsert:
                        return clipboardPasteOutput.TryPasteText(data, "+{INSERT}", out error);
                    case TextOutputMode.PasteCtrlV:
                        return clipboardPasteOutput.TryPasteText(data, "^{v}", out error);
                    case TextOutputMode.PasteBig5:
                        return clipboardPasteOutput.TryPasteAnsiText(my.UTF8toBig5(data), "^{v}", out error);
                    case TextOutputMode.TsfBridge:
                        return tsfBridgeOutput.TryCommitText(data, foregroundProcessId, get_tsf_bridge_timeout_ms(), out error);
                    default:
                        error = "unknown output mode: " + outputMode;
                        return false;
                }
            }
            finally
            {
                is_send_ucl = false;
            }
        }

        private bool try_send_legacy_sendkeys(string data, out string error)
        {
            error = null;
            is_send_ucl = true;
            try
            {
                SendKeys.Send(data);
                return true;
            }
            catch (Exception ex)
            {
                error = "legacy SendKeys failed: " + ex.Message;
                return false;
            }
            finally
            {
                is_send_ucl = false;
            }
        }

        private bool is_windows11_or_later()
        {
            if (!isWindows11OrLaterCache.HasValue)
            {
                isWindows11OrLaterCache = WindowsVersionDetector.IsWindows11OrLater();
            }
            return isWindows11OrLaterCache.Value;
        }

        private int get_tsf_bridge_timeout_ms()
        {
            return TsfBridgeSettings.NormalizeTimeout(config["DEFAULT"]["TSF_BRIDGE_TIMEOUT_MS"], TsfBridgeConstants.DefaultTimeoutMilliseconds);
        }

        private string prepare_senddata_text(string data)
        {
            data = data ?? "";
            same_sound_index = 0;// #回到第零頁
            is_has_more_page = false;// #回到沒有分頁
            same_sound_last_word = "";
            is_need_use_phone = false;
            play_ucl_label = "";
            ucl_find_data = new List<string>();
            outputHintComposer.BeginOutput();
            type_label_set_text();

            if (is_simple())
            {
                //如果需要殘體，正轉殘
                data = trad2simple(data);
            }

            return data;
        }

        public void senddata(string data)
        {
            data = prepare_senddata_text(data);
            send_prepared_output(data);
        }

        private void send_prepared_output(string data)
        {
            //人生很難，研究很久 C# 的 sendkeys 遇到有些吃 iso-8859-1、big5 的app 如pcman、putty
            //或是早期的 photoimpact，最好的方法還是利用剪貼簿貼上，使用前備份一下原來的內容即可
            //一般視窗先使用 Unicode SendInput；剪貼簿仍保留給終端機、Big5、特殊 App。
            //data = "肥的天下";
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
            TextOutputContext outputContext = new TextOutputContext(p_info["PROCESS_NAME"], p_info["PROCESS_TITLE"], is_windows11_or_later());
            TextOutputMode outputMode = TextOutputRouter.Select(DEFAULT_OUTPUT_TYPE, outputContext, sendkey_paste_shift_ins_apps, sendkey_paste_ctrl_v_apps, sendkey_paste_big5_apps);
            debug_print("senddata output mode:" + outputMode.ToString());
            int foregroundProcessId = 0;
            Int32.TryParse(p_info["PROCESS_PID"], out foregroundProcessId);

            string outputError;
            if (try_send_output(outputMode, data, foregroundProcessId, out outputError))
            {
                return;
            }

            debug_print("senddata primary output failed:" + outputError);
            if (outputMode != TextOutputMode.UnicodeSendInput && outputMode != TextOutputMode.PasteBig5)
            {
                if (try_send_output(TextOutputMode.UnicodeSendInput, data, foregroundProcessId, out outputError))
                {
                    return;
                }
                debug_print("senddata unicode fallback failed:" + outputError);
            }

            if (!try_send_legacy_sendkeys(data, out outputError))
            {
                debug_print("senddata legacy fallback failed:" + outputError);
            }

        }
    }
}
