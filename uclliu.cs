﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Windows.Forms;
//using System.Threading;
using System.Drawing;
using IniParser;
using IniParser.Model;
using System.Web.Helpers;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters;
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
        public bool run_extra() //跑額外的功能，如 ,,,version
        {
            string code = "";
            Console.WriteLine("last_key:" + last_key);
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
            _msg_text += "「,,,T」繁體模式\n";
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

            //簡繁
            if (is_simple())
            {
                //簡模式
                f.LP.ColumnStyles[4] = new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, Convert.ToInt32(40 * Convert.ToDouble(config["DEFAULT"]["ZOOM"])));
            }
            else
            {
                //繁模式
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
                debug_print("Crash...");
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
        public string trad2simple(string data)
        {
            //正體轉殘體
            //todo
            return data;
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
                //如果是簡體，繁轉簡
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
