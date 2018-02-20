using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Optimization
{
    public partial class OptForm : Form
    {
        public OptForm()
        {
            InitializeComponent();
        }

        public double Temperature;

        public struct str
        {
            public double G;
            public double t;
        }

        public struct pokazateli
        {
            public double lambda;
            public double alfa;
            public double C;
        }

        public struct str1
        {
            public int lower;
            public int upper;
        }

        public struct rezult1
        {
            public double t;
            public double g;
            public double R;
            public double alfa;
            public double lambda;
            public double C;
        }

        public struct point
        {
            public double t;
            public double g;
        }

        public point C;
        public rezult1 rezult;
        public str1 Tstr, Gstr;
        public str Gt;


        public double alf(double Top, double t0, double tmin, double tmax, double gam)
        {
            double alfa = 0;
            alfa = 1 / (7.834 + 0.519 * Math.Pow((Top - t0) / (tmax - tmin), 0.374));
            alfa = alfa / (0.145 + 0.0009 * ((Top - t0) / (tmax - tmin)) - 1.362 * Math.Pow(2.719, -6) * Math.Pow((Top - t0) / (tmax - tmin), 2) + 8.227 * Math.Pow(2.719, -10) * Math.Pow((Top - t0) / (tmax - tmin), 3));
            alfa = alfa / gam;
            return alfa;
        }

        public double tshop(double tau)
        {
            double TT, a = 1077.856, b = 8.797, c = 0.716, d = 2.687;
            TT = a / Math.Pow((1 + Math.Pow(2.719, b - c * tau)), 1 / d);
            return TT;
        }

        public double Get_temperature(double tproc, double Tst)
        {
            double temp = 0;
            double na = 1;
            double Tmin, Tmax, Tshn, x, L, tau, ak = 0, Ak1, Ak2, sigma, gamma, alfa, Tshop;
            double[] T0 = new double[101];
            int k, n;

            T0[1] = double.Parse(textBox3.Text);

            L = double.Parse(textBox4.Text) * 2.5;

            Tst = Tst * na / 19;
            tau = 1;

            x = double.Parse(textBox5.Text) * 2.5;

            gamma = 6;
            sigma = 0.5;
            T0[0] = Tst;
            Tmin = T0[1];
            Tmax = Tst;
            Tshop = T0[1];
            Tshn = Tmin;
            double h;

            int kkk = Convert.ToInt32((L / 2) / x);
            k = kkk + 2;
            double z = (1 / k);

            h = k;
            h = 1 / h;
            n = Convert.ToInt32(tproc * 14 / tau);

            double[] teta0 = new double[101];
            double[] teta1 = new double[101];

            teta0[0] = (Tst - T0[1]) / (Tmax - Tmin);
            int i = 1;

            while (i < k)
            {
                teta0[i] = 0;
                T0[i] = Tshn;
                i++;
            }

            teta1[0] = teta0[0];
            teta0[k] = teta0[k - 1];
            teta1[k] = teta0[k];
            T0[k] = T0[k - 1];

            int j = 0;
            double alfa1, alfa2 = 0;
            alfa = 1.9;
            Ak1 = sigma * tau * ak / (Math.Pow(h, 2));
            ak = alfa2;
            Ak2 = sigma * tau * ak / (Math.Pow(h, 2));

            while (j < n)
            {
                Tshop = tshop(tau * (j + 1));
                alfa2 = alf(teta0[1] * (Tmax - Tmin) + T0[i], T0[1], Tmin, Tmax, gamma);
                alfa1 = alfa;
                ak = 0.5 * (alfa1 + alfa2);
                Ak1 = sigma * tau * (n + 1) * ak / (Math.Pow(h, 2));
                alfa1 = alf(teta0[2] * (Tmax - Tmin) + T0[i], T0[1], Tmin, Tmax, gamma);
                ak = 0.5 * (alfa2 + alfa1);
                Ak2 = sigma * tau * (n + 1) * ak / (Math.Pow(h, 2));
                i = 1;
                while (i < k)
                {
                    teta1[i] = (teta1[i - 1] * Ak1 - teta0[i] / tau + teta0[i + 1] * Ak2) / (1 + Ak1 + Ak2);
                    alfa1 = alf(teta1[i] * (Tmax - Tmin) + T0[1], T0[1], Tmin, Tmax, gamma);
                    alfa2 = alf(teta0[i + 1] * (Tmax - Tmin) + T0[1], T0[1], Tmin, Tmax, gamma);
                    ak = 0.5 * (alfa1 + alfa2);
                    Ak1 = sigma * tau * (n + 1) * ak / (Math.Pow(h, 2));
                    alfa1 = alf(teta0[i + 2] * (Tmax - Tmin) + T0[1], T0[1], Tmin, Tmax, gamma);
                    ak = 0.5 * (alfa1 + alfa2);
                    Ak2 = sigma * tau * (n + 1) * ak / (Math.Pow(h, 2));
                    i++;
                }

                i = 1;
                teta0[k] = teta1[k - 1];
                teta1[k] = teta0[k];
                while (i < k)
                {
                    teta0[i] = teta1[i];
                    double tt;
                    tt = teta1[i] * (Tmax - Tmin) + T0[i];
                    i++;
                }

                //температура в осевой плоскости пирога
                temp = teta0[k] * (Tmax - Tmin) + T0[1];
                j++;
            }
            return temp;
        }

        public double Get_R(double temperature, double C1, double C2, double C3, pokazateli pok)
        {
            double R;
            double alfa;
            double C;
            double lambda;
            alfa = 0.0002648 + 0.0000015 * temperature + 0.000000001559 * Math.Pow(temperature, 2);//0.00058876877*exp(0.00175*temperature);
            lambda = 0.12536 + 0.00017796 * temperature + 0.000000726 * Math.Pow(temperature, 2);//exp(-15.619+584.21895/temperature+2.1796994*log(temperature)/log(2.71828));//1/(17.474-2.3913*(log(temperature)/log(2.71828)));
            C = lambda / (alfa * 6);
            R = C1 * Math.Pow(lambda - pok.lambda, 2) / pok.lambda + C2 * Math.Pow(C - pok.C, 2) / pok.C + C3 * Math.Pow(alfa - pok.alfa, 2) / pok.alfa;
            rezult.alfa = alfa;
            rezult.lambda = lambda;
            rezult.C = C;
            return R;
        }

        public void Box(str1 Gstr, str1 Tstr, double C1, double C2, double C3, pokazateli pok)
        {
            int n = 2;
            int N = 2 * n;
            double[] F = new double[4];
            double Ftmp;
            double B = 1000;
            double count = 0;
            double eps = double.Parse(textBox13.Text);
            int max_iter_count = int.Parse(textBox14.Text);

            point[] points = new point[4];
            // формирование исходного комплекса
            points[0].t = 0; points[0].g = Gstr.lower;
            points[1].t = 0; points[1].g = Gstr.upper;
            points[2].t = 26; points[2].g = Gstr.lower;
            points[3].t = 26; points[3].g = Gstr.upper;

            for (int i = 0; i < 4; i++)
            {
                F[i] = Get_R(Get_temperature(points[i].t, points[i].g), C1, C2, C3, pok);
            }

            int G = 0; //номер самой хорошей вершины
            int D = 0; //номер самой плохой вершины
            double Fd = F[0]; //наихудшее значение
            double Fg = F[0]; //наилучшее значение

            while (B > eps && count < max_iter_count)
            {
                G = 0; D = 0;
                Fd = F[0]; Fg = F[0];
                //поиск номера наилучшей вершины
                for (int i = 1; i < 4; i++)
                {
                    if (Fg > F[i])
                    {
                        G = i;
                        Fg = F[i];
                    }
                }

                //поиск номера наихудщей вершины
                for (int i = 1; i < 4; i++)
                {
                    if (Fd < F[i])
                    {
                        D = i;
                        Fd = F[i];
                    }
                }

                //Определение координат центра Комплекса
                C.t = 1.0 / (N - 1) * (points[0].t + points[1].t + points[2].t + points[3].t - points[D].t);
                C.g = 1.0 / (N - 1) * (points[0].g + points[1].g + points[2].g + points[3].g - points[D].g);

                //Проверка условия окончания поиска
                B = 1.0 / (2 * n) * ((Math.Abs(C.t - points[D].t) + Math.Abs(C.t - points[G].t))
                + (Math.Abs(C.g - points[D].g) + Math.Abs(C.g - points[G].g)));


                if (B < eps)
                {
                    rezult.t = points[G].t;
                    rezult.g = points[G].g;
                    rezult.R = Fg;
                    break;
                }
                else
                //вычисление координаты новой точки Комплекса взамен наихудшей
                {
                    points[D].t = 2.3 * C.t - 1.3 * points[D].t;
                    points[D].g = 2.3 * C.g - 1.3 * points[D].g;

                    //проверка выполнения ограничений первого рода
                    if (points[D].t < Tstr.lower) points[D].t = Tstr.lower + eps;
                    if (points[D].t > Tstr.upper) points[D].t = Tstr.upper - eps;
                    if (points[D].g < Gstr.lower) points[D].g = Gstr.lower + eps;
                    if (points[D].g > Gstr.upper) points[D].g = Gstr.upper - eps;

                    //Вычисление значения целевой функции в новой точке
                    Ftmp = Get_R(Get_temperature(points[D].t, points[D].g), C1, C2, C3, pok);

                    while (Ftmp > F[D])
                    {
                        points[D].t = (points[D].t + points[G].t) / 2.0;
                        points[D].g = (points[D].g + points[G].g) / 2.0;
                        Ftmp = Get_R(Get_temperature(points[D].t, points[D].g), C1, C2, C3, pok);
                    }
                    F[D] = Ftmp;
                }
                count++;
            }
        }

        private void OptForm_Load(object sender, EventArgs e)
        {
           
        }

        private void методБоксаToolStripMenuItem_Click(object sender, EventArgs e)
        {

        }

        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                pokazateli pok;
                double C1, C2, C3;

                C1 = double.Parse(textBox10.Text);
                C2 = double.Parse(textBox11.Text);
                C3 = double.Parse(textBox12.Text);

                Tstr.lower = 0;
                Tstr.upper = int.Parse(textBox7.Text);

                Gstr.lower = int.Parse(textBox8.Text);
                Gstr.upper = int.Parse(textBox9.Text);

                pok.lambda = double.Parse(textBox15.Text);
                pok.C = double.Parse(textBox17.Text);
                pok.alfa = double.Parse(textBox16.Text);

                Box(Gstr, Tstr, C1, C2, C3, pok);

                textBox21.Text = Math.Round(rezult.lambda, 2).ToString();
                textBox22.Text = Math.Round(rezult.alfa, 5).ToString();
                textBox23.Text = Math.Round(rezult.C, 2).ToString();

                textBox18.Text = Math.Round(rezult.R, 2).ToString();
                textBox19.Text = Math.Round(rezult.g + 1, 2).ToString();
                textBox20.Text = Math.Round(rezult.t, 2).ToString();

                int x = 0;
                int y = 0;
                double rez;

                dataGridView1.ColumnCount = (Tstr.upper - Tstr.lower) - 1;
                dataGridView1.RowCount = ((Gstr.upper - Gstr.lower) / 200) + 1;
                dataGridView2.ColumnCount = (Tstr.upper - Tstr.lower) - 1;
                dataGridView2.RowCount = ((Gstr.upper - Gstr.lower) / 200) + 1;

                for (int i = Tstr.lower + 1; i < Tstr.upper; i++)
                {
                    x = 0;

                    for (int j = Gstr.lower; j <= Gstr.upper; j = j + 200)
                    {
                        rez = Get_temperature(i, j);

                        dataGridView1.Rows[x].HeaderCell.Value = j.ToString();
                        dataGridView1.RowHeadersWidth = 80;

                        dataGridView2.Rows[x].HeaderCell.Value = j.ToString();
                        dataGridView2.RowHeadersWidth = 80;

                        dataGridView1[y, x].Value = Math.Round(rez, 1);
                        dataGridView2[y, x].Value = Math.Round(Get_R(rez, C1, C2, C3, pok), 3);

                        x++;
                    }

                    dataGridView1.Columns[y].HeaderText = "" + (i);
                    dataGridView1.Columns[y].Width = 60;

                    dataGridView2.Columns[y].HeaderText = "" + (i);
                    dataGridView2.Columns[y].Width = 60;
                    y++;
                }
                tabControl1.SelectedTab = tabControl1.TabPages["tabPage2"];
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message, "Ошибка", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            
        }
    }
}
