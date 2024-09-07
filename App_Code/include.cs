using System;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Collections;
using System.Json;

namespace utility
{
    public class myinclude
    {

        public string pwd()
        {
            return Directory.GetCurrentDirectory();
        }
        public bool is_dir(string path)
        {
            return Directory.Exists(path);
        }
        public bool is_file(string filepath)
        {
            return File.Exists(filepath);
        }
        public string trim(string data)
        {
            return data.Trim();
        }
        public void unlink(string filepath)
        {
            if (is_file(filepath))
            {
                File.Delete(filepath);
            }
        }
        public bool in_array(string find_key, List<string> arr)
        {
            return arr.Contains(find_key);
        }
        public bool in_array(string find_key, string[] arr)
        {
            return arr.Contains(find_key);
        }
        public string implode(string keyword, string[] arrays)
        {
            return string.Join(keyword, arrays);
        }
        public string implode(string keyword, List<string> arrays)
        {
            return string.Join<string>(keyword, arrays);
        }
        public string implode(string keyword, Dictionary<int, string> arrays)
        {
            string[] tmp = new String[arrays.Keys.Count];
            int i = 0;
            foreach (int k in arrays.Keys)
            {
                tmp[i++] = arrays[k];
            }
            return string.Join(keyword, tmp);
        }
        public string implode(string keyword, Dictionary<string, string> arrays)
        {
            string[] tmp = new String[arrays.Keys.Count];
            int i = 0;
            foreach (string k in arrays.Keys)
            {
                tmp[i++] = arrays[k];
            }
            return string.Join(keyword, tmp);
        }
        public string implode(string keyword, ArrayList arrays)
        {
            string[] tmp = new String[arrays.Count];
            for (int i = 0; i < arrays.Count; i++)
            {
                tmp[i] = arrays[i].ToString();
            }
            return string.Join(keyword, tmp);
        }
        public List<string> jsonValueToListString(JsonValue data)
        {
            List<string> o = new List<string>();
            if (data.ToString().Contains("["))
            {
                for (int i = 0, max_i = data.Count; i < max_i; i++)
                {
                    o.Add(data[i].ToString().Replace("\"", ""));
                }
            }
            else
            {
                o.Add(data.ToString().Replace("\"", ""));
            }
            return o;
        }
        public string UTF8toBig5(string strInput)
        {
            byte[] strut8 = System.Text.Encoding.Unicode.GetBytes(strInput);
            byte[] strbig5 = System.Text.Encoding.Convert(System.Text.Encoding.Unicode, System.Text.Encoding.Default, strut8);
            return System.Text.Encoding.Default.GetString(strbig5);
        }
        public string UTF8toCP950(string strInput)
        {
            byte[] unknow = System.Text.Encoding.Default.GetBytes(strInput);
            string Big5 = System.Text.Encoding.Default.GetString(unknow);
            return Big5;

        }
        public bool in_array(string find_key, ArrayList arr)
        {
            return arr.Contains(find_key);
        }
        public bool in_array(string find_key, JsonValue arr)
        {
            return arr.ContainsKey(find_key);
        }
        public bool is_string_like(string data, string find_string)
        {
            return (data.IndexOf(find_string) == -1) ? false : true;
        }
        //大小寫
        public string strtoupper(string input)
        {
            return input.ToUpper();
        }
        public string strtolower(string input)
        {
            return input.ToLower();
        }

        public string[] explode(string keyword, string data)
        {
            return data.Split(new string[] { keyword }, StringSplitOptions.None);
        }        
        public string[] array_unique(string[] s)
        {
            //去除重複
            HashSet<string> set = new HashSet<string>(s);
            string[] result = new string[set.Count];
            set.CopyTo(result);
            return result;
        }
        public string[] explode(string[] keyword, string data)
        {
            return data.Split(keyword, StringSplitOptions.None);
        }
        public byte[] s2b(string input)
        {
            return System.Text.Encoding.UTF8.GetBytes(input);
        }
        public string b2s(byte[] input)
        {
            return System.Text.Encoding.UTF8.GetString(input);
        }
        public void file_put_contents(string filepath, string input)
        {
            file_put_contents(filepath, s2b(input));
        }
        public void file_put_contents(string filepath, byte[] input)
        {
            FileStream myFile = File.Open(@filepath, FileMode.Create);
            myFile.Write(input, 0, input.Length);
            myFile.Dispose();
        }
        public byte[] file_get_contents(string path)
        {
            System.IO.StreamReader sr = new System.IO.StreamReader(path);
            string sContents = sr.ReadToEnd();
            sr.Close();
            return s2b(sContents);
        }            
        public string basename(string path)
        {
            return Path.GetFileName(path);
        }
        public string mainname(string path)
        {
            return Path.GetFileNameWithoutExtension(path);
        }
        public string subname(string path)
        {
            return Path.GetExtension(path);
        }
       

        public void mkdir(string path)
        {
            Directory.CreateDirectory(path);
        }
        public void copy(string sourceFile, string destFile)
        {
            System.IO.File.Copy(sourceFile, destFile, true);
        }
        public string dirname(string path)
        {
            return Directory.GetParent(path).FullName;
        }
       
        
    }

}