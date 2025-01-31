package utils.math.linalg;

import utils.math.space.V3;

import java.util.Arrays;

public class MatrixUtils {

    /***/
    public static double[][] mullSquareMatrices(double[][] a, double[][] b) {
        final int n = a.length;
        double res[][] = new double[n][n];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++) {
                    res[i][j] += a[i][k] * b[k][j];
                }
            }
        }
        return res;
    }

   /* public static V2 mull(double[][] a, V2 v) {
        return new V2(
                v.x() * a[0][0] + v.y() * a[0][1],
                v.x() * a[1][0] + v.y() * a[1][1]);
    }
*/
    public static V3 mull(double[][] a, V3 v) {
        return new V3(
                v.x() * a[0][0] + v.y() * a[0][1] + v.z() * a[0][2],
                v.x() * a[1][0] + v.y() * a[1][1] + v.z() * a[1][2],
                v.x() * a[2][0] + v.y() * a[2][1] + v.z() * a[2][2]);
    }

    public static double[] mull(double[][] a, double[] v) {
        double res[] = new double[v.length];
        for (int i = 0; i < a.length; i++) {
            for (int j = 0; j < v.length; j++) {
                res[i] += a[i][j] * v[j];
            }
        }
        return res;
    }

    //diagonal matrix
    public static double[][] scaleMatrix(double[] scaleCoeffs) {
        double[][] res = new double[scaleCoeffs.length][scaleCoeffs.length];
        for (int i = 0; i < scaleCoeffs.length; i++) {
            res[i][i] = scaleCoeffs[i];
        }
        return res;
    }

   /* public static double[][] scaleMatrix(V2 v) {
        return scaleMatrix(new double[]{v.x(), v.y()});
    }

    public static double[][] scaleMatrix(V3 v) {
        return scaleMatrix(new double[]{v.x(), v.y(), v.z()});
    }*/

    public static double[][] rotationMatrix2x2(double a) {
        return new double[][]{
                {(double) Math.cos(a), (double) -Math.sin(a)},
                {(double) Math.sin(a), (double) Math.cos(a)},
        };
    }

    public static double[][] rotationMatrix2x2Homo(double a) {
        return new double[][]{
                {(double) Math.cos(a), (double) -Math.sin(a), 0},
                {(double) Math.sin(a), (double) Math.cos(a), 0},
                {0, 0, 1},
        };
    }

    /**
     * @param axises оси и перемещение по ним
     * @return матрица трансфомации для работы в гомогенных координатах
     */
    public static double[][] homoTransformMatrix(double[] axises) {
        double[][] res = new double[axises.length + 1][axises.length + 1];
        for (int i = 0; i < axises.length; i++) {
            res[i][axises.length] = axises[i];
        }
        for (int i = 0; i < res.length; i++) {
            res[i][i] = 1;
        }
        return res;
    }

    public static double[][] toBasis(double[] x, double[] y, double[] z) {
        double res[][] = new double[3][3];
        for (int i = 0; i < 3; i++) {
            res[0][i] = x[i];
            res[1][i] = y[i];
            res[2][i] = z[i];
        }
        return res;
    }

    public static double[][] toHomo(double[][] m) {
        double[][] homo = new double[m.length + 1][m[0].length + 1];
        homo[m.length][m[0].length] = 1;
        for (int i = 0; i < m.length; i++) {
            for (int j = 0; j < m[i].length; j++) {
                homo[i][j] = m[i][j];
            }
        }
        return homo;
    }

    /*public static double[][] lookAt(V3 eye, V3 center, V3 up) {
        V3 z = eye.$minus(center).normalize();
        V3 x = up.$up(z).normalize();
        V3 y = z.$up(x);
        double[][] basis = toHomo(toBasis(x, y, z));
        double[][] translate = homoTransformMatrix(center.toArray());
        return mull(basis, translate);
    }*/

    public static double[][] identity(int size) {
        double res[][] = new double[size][size];
        for (int i = 0; i < size; i++) {
            res[i][i] = 1;
        }
        return res;
    }

    public static double[][] viewport(double x, double y, double w, double h, double depth) {
        double[][] m = identity(4);
        m[0][3] = x + w / 2.f;
        m[1][3] = y + h / 2.f;
        m[2][3] = depth / 2.f;

        m[0][0] = w / 2.f;
        m[1][1] = h / 2.f;
        m[2][2] = depth / 2.f;
        return m;
    }

    public static double[][] cameraZPr(double cameraPosition) {
        return new double[][]{
                {1, 0, 0, 0},
                {0, 1, 0, 0},
                {0, 0, 1, 0},
                {0, 0, -1 / cameraPosition, 1}
        };
    }

    public static double[][] rotateX(double a) {
        return new double[][]{
                {1, 0, 0, 0},
                {0, (double) Math.cos(a), (double) -Math.sin(a), 0},
                {0, (double) Math.sin(a), (double) Math.cos(a), 0},
                {0, 0, 0, 1}
        };
    }

    public static double[][] rotateY(double a) {
        return new double[][]{
                {(double) Math.cos(a), 0, (double) Math.sin(a), 0},
                {0, 1, 0, 0},
                {(double) -Math.sin(a), 0, (double) Math.cos(a), 0},
                {0, 0, 0, 1}
        };
    }

    public static double[][] rotateZ(double a) {
        return new double[][]{
                {(double) Math.cos(a), (double) -Math.sin(a), 0, 0},
                {(double) Math.sin(a), (double) Math.cos(a), 0, 0},
                {0, 0, 1, 0},
                {0, 0, 0, 1}
        };
    }

    public static double[][] transpose(double[][] m) {
        double[][] res = new double[m[0].length][m.length];
        for (int i = 0; i < m.length; i++) {
            for (int j = 0; j < m[0].length; j++) {
                res[j][i] = m[i][j];
            }
        }
        return res;
    }


    public static boolean compare(double[][] m, double [][] m2) {
        return Arrays.deepEquals(m, m2);
    }

    public static double[][] inverse4(double[][] m) {
        return new double[][]{
                {
                        m[1][2] * m[2][3] * m[3][1] - m[1][3] * m[2][2] * m[3][1] + m[1][3] * m[2][1] * m[3][2] - m[1][1] * m[2][3] * m[3][2] - m[1][2] * m[2][1] * m[3][3] + m[1][1] * m[2][2] * m[3][3],
                        m[0][3] * m[2][2] * m[3][1] - m[0][2] * m[2][3] * m[3][1] - m[0][3] * m[2][1] * m[3][2] + m[0][1] * m[2][3] * m[3][2] + m[0][2] * m[2][1] * m[3][3] - m[0][1] * m[2][2] * m[3][3],
                        m[0][2] * m[1][3] * m[3][1] - m[0][3] * m[1][2] * m[3][1] + m[0][3] * m[1][1] * m[3][2] - m[0][1] * m[1][3] * m[3][2] - m[0][2] * m[1][1] * m[3][3] + m[0][1] * m[1][2] * m[3][3],
                        m[0][3] * m[1][2] * m[2][1] - m[0][2] * m[1][3] * m[2][1] - m[0][3] * m[1][1] * m[2][2] + m[0][1] * m[1][3] * m[2][2] + m[0][2] * m[1][1] * m[2][3] - m[0][1] * m[1][2] * m[2][3]},
                {
                        m[1][3] * m[2][2] * m[3][0] - m[1][2] * m[2][3] * m[3][0] - m[1][3] * m[2][0] * m[3][2] + m[1][0] * m[2][3] * m[3][2] + m[1][2] * m[2][0] * m[3][3] - m[1][0] * m[2][2] * m[3][3],
                        m[0][2] * m[2][3] * m[3][0] - m[0][3] * m[2][2] * m[3][0] + m[0][3] * m[2][0] * m[3][2] - m[0][0] * m[2][3] * m[3][2] - m[0][2] * m[2][0] * m[3][3] + m[0][0] * m[2][2] * m[3][3],
                        m[0][3] * m[1][2] * m[3][0] - m[0][2] * m[1][3] * m[3][0] - m[0][3] * m[1][0] * m[3][2] + m[0][0] * m[1][3] * m[3][2] + m[0][2] * m[1][0] * m[3][3] - m[0][0] * m[1][2] * m[3][3],
                        m[0][2] * m[1][3] * m[2][0] - m[0][3] * m[1][2] * m[2][0] + m[0][3] * m[1][0] * m[2][2] - m[0][0] * m[1][3] * m[2][2] - m[0][2] * m[1][0] * m[2][3] + m[0][0] * m[1][2] * m[2][3]},
                {
                        m[1][1] * m[2][3] * m[3][0] - m[1][3] * m[2][1] * m[3][0] + m[1][3] * m[2][0] * m[3][1] - m[1][0] * m[2][3] * m[3][1] - m[1][1] * m[2][0] * m[3][3] + m[1][0] * m[2][1] * m[3][3],
                        m[0][3] * m[2][1] * m[3][0] - m[0][1] * m[2][3] * m[3][0] - m[0][3] * m[2][0] * m[3][1] + m[0][0] * m[2][3] * m[3][1] + m[0][1] * m[2][0] * m[3][3] - m[0][0] * m[2][1] * m[3][3],
                        m[0][1] * m[1][3] * m[3][0] - m[0][3] * m[1][1] * m[3][0] + m[0][3] * m[1][0] * m[3][1] - m[0][0] * m[1][3] * m[3][1] - m[0][1] * m[1][0] * m[3][3] + m[0][0] * m[1][1] * m[3][3],
                        m[0][3] * m[1][1] * m[2][0] - m[0][1] * m[1][3] * m[2][0] - m[0][3] * m[1][0] * m[2][1] + m[0][0] * m[1][3] * m[2][1] + m[0][1] * m[1][0] * m[2][3] - m[0][0] * m[1][1] * m[2][3]},
                {
                        m[1][2] * m[2][1] * m[3][0] - m[1][1] * m[2][2] * m[3][0] - m[1][2] * m[2][0] * m[3][1] + m[1][0] * m[2][2] * m[3][1] + m[1][1] * m[2][0] * m[3][2] - m[1][0] * m[2][1] * m[3][2],
                        m[0][1] * m[2][2] * m[3][0] - m[0][2] * m[2][1] * m[3][0] + m[0][2] * m[2][0] * m[3][1] - m[0][0] * m[2][2] * m[3][1] - m[0][1] * m[2][0] * m[3][2] + m[0][0] * m[2][1] * m[3][2],
                        m[0][2] * m[1][1] * m[3][0] - m[0][1] * m[1][2] * m[3][0] - m[0][2] * m[1][0] * m[3][1] + m[0][0] * m[1][2] * m[3][1] + m[0][1] * m[1][0] * m[3][2] - m[0][0] * m[1][1] * m[3][2],
                        m[0][1] * m[1][2] * m[2][0] - m[0][2] * m[1][1] * m[2][0] + m[0][2] * m[1][0] * m[2][1] - m[0][0] * m[1][2] * m[2][1] - m[0][1] * m[1][0] * m[2][2] + m[0][0] * m[1][1] * m[2][2]}
        };
    }
}

